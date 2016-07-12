{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Two space stop-and-copy garbage collector.
--
-- * Compacting: memory is moved to a new location
-- * Tracing
module Stg.Machine.GarbageCollection.TwoSpaceCopying (
    twoSpaceCopying,
) where



import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Monoid
import           Data.Sequence              (Seq, ViewL (..), (|>))
import qualified Data.Sequence              as Seq
import           Data.Set                   (Set)
import qualified Data.Set                   as S

import           Stg.Machine.GarbageCollection.Common
import qualified Stg.Machine.Heap                     as H
import           Stg.Machine.Types



-- | Remove all unused addresses by moving them to a safe location.
twoSpaceCopying :: GarbageCollectionAlgorithm
twoSpaceCopying = GarbageCollectionAlgorithm
    "Two-space copying"
    garbageCollect

newtype Gc a = Gc (ReaderT Heap (State GcState) a)
    deriving (Functor, Applicative, Monad)

askHeap :: Gc Heap
askHeap = Gc ask

getGcState :: Gc GcState
getGcState = Gc (lift get)

putGcState :: GcState -> Gc ()
putGcState s = Gc (lift (put s))

execGc :: Gc a -> Heap -> GcState -> GcState
execGc (Gc rsa) oldHeap gcState =
    let sa = runReaderT rsa oldHeap
        finalState = execState sa gcState
    in finalState

data GcState = GcState
    { toHeap :: Heap
        -- ^ Heap of closures known to be alive.

    , forwards :: Map MemAddr MemAddr
        -- ^ Forward pointers to the new locations of already collected heap
        -- objects

    , toScavenge :: Seq MemAddr
        -- ^ Heap objects already evacuated, but not yet scavenged. Contains
        -- only objects that are also in the @toHeap@.

    , toEvacuate :: Seq MemAddr
        -- ^ Heap objects known to be alive, but not yet evacuated.
    } deriving (Eq, Ord, Show)

garbageCollect :: StgState -> (Set MemAddr, Map MemAddr MemAddr, StgState)
garbageCollect stgState@StgState
    { stgCode    = code
    , stgHeap    = heap
    , stgGlobals = globals
    , stgStack   = stack }
  = let rootAddrs = mconcat [addrs code, addrs stack, addrs globals]
        initialState = GcState
            { toHeap     = mempty
            , forwards   = mempty
            , toScavenge = mempty
            , toEvacuate = rootAddrs }
        finalState = execGc evacuateScavengeLoop heap initialState
    in case finalState of
        GcState {toHeap = heap', forwards = forwards'} ->
            let deadFormerAddrs
                  = let Heap old = heap
                    in M.keysSet old `S.difference` M.keysSet forwards'

                forward addr = M.findWithDefault forwardErr addr forwards'
                forwardErr = error "Invalid forward in GC; please report this as a bug"

                removeIdentities = M.filterWithKey (/=)

                stgState' = stgState
                    { stgCode    = updateAddrs forward code
                    , stgStack   = updateAddrs forward stack
                    , stgGlobals = updateAddrs forward globals
                    , stgHeap    = heap' }
            in (deadFormerAddrs, removeIdentities forwards', stgState')

evacuateScavengeLoop :: Gc ()
evacuateScavengeLoop = initialEvacuation >> scavengeLoop

initialEvacuation :: Gc ()
initialEvacuation = getAndClearToEvacuate >>= evacuateAll
  where
    getAndClearToEvacuate = do
        gcState <- getGcState
        putGcState (gcState{toEvacuate = mempty})
        pure (toEvacuate gcState)
    evacuateAll = traverse_ evacuate

scavengeLoop :: Gc ()
scavengeLoop = do
    scavengeNext <- getAndClearToScavenge
    if | Seq.null scavengeNext -> pure ()
       | otherwise -> do
           scavengeAddrs S.empty scavengeNext
           scavengeLoop
  where
    getAndClearToScavenge = do
        gcState <- getGcState
        putGcState (gcState{toScavenge = mempty})
        pure (toScavenge gcState)
    scavengeAddrs alreadyScavenged toAddrs = case Seq.viewl toAddrs of
        EmptyL -> pure ()
        addr :< rest
            | S.member addr alreadyScavenged -> scavengeAddrs alreadyScavenged rest
            | otherwise -> do
                scavenge addr
                scavengeAddrs (S.insert addr alreadyScavenged) rest

data EvacuationStatus = NotEvacuatedYet | AlreadyEvacuatedTo MemAddr

-- | Copy a closure from from-space to to-space, if it has not been evacuated
-- previously. Copying to to-space involves a new allocation in to-space,
-- registering the copied address to be scavenged, and creating a forwarding
-- entry so that further evacuations can short-circuit.
evacuate :: MemAddr -> Gc MemAddr
evacuate = \fromAddr -> forwardingStatus fromAddr >>= \case
    AlreadyEvacuatedTo newAddr -> pure newAddr
    NotEvacuatedYet -> fmap (H.lookup fromAddr) askHeap >>= \case
        Nothing -> error "Tried collecting a non-existent memory address!\
                         \ Please report this as a bug."
        Just heapObject -> do
            newAddr <- copyIntoToSpace heapObject
            registerToBeScavenged newAddr
            createForward fromAddr newAddr
            pure newAddr
  where
    forwardingStatus :: MemAddr -> Gc EvacuationStatus
    forwardingStatus addr = do
        GcState { forwards = forw } <- getGcState
        pure (case M.lookup addr forw of
            Nothing -> NotEvacuatedYet
            Just newAddr -> AlreadyEvacuatedTo newAddr )

    copyIntoToSpace :: HeapObject -> Gc MemAddr
    copyIntoToSpace heapObject = do
        gcState <- getGcState
        let (addr', to') = H.alloc heapObject (toHeap gcState)
        putGcState gcState { toHeap = to' }
        pure addr'

    registerToBeScavenged :: MemAddr -> Gc ()
    registerToBeScavenged addr = do
        gcState@GcState { toScavenge = sc } <- getGcState
        putGcState gcState { toScavenge = sc |> addr }

    createForward :: MemAddr -> MemAddr -> Gc ()
    createForward from to = do
        gcState@GcState{forwards = forw} <- getGcState
        putGcState gcState { forwards = M.insert from to forw }

-- | Find referenced addresses in a heap object, and overwrite them with their
-- evacuated new addresses.
scavenge :: MemAddr -> Gc ()
scavenge = \scavengeAddr -> do
    scavengeHeapObject <- do
        GcState { toHeap = heap } <- getGcState
        pure (H.lookup scavengeAddr heap)
    case scavengeHeapObject of
        Nothing -> error "Scavenge error: address not found on to-heap\
                         \ Please report this as a bug."
        Just Blackhole{} -> pure mempty
        Just (HClosure (Closure lf frees)) -> do
            frees' <- evacuateContainedValues frees
            updateClosure scavengeAddr (Closure lf frees')
            registerForEvacuation [ addr | Addr addr <- frees' ]
  where
    evacuateContainedValues :: [Value] -> Gc [Value]
    evacuateContainedValues = traverse (\case
        Addr addr -> fmap (\x -> Addr x) (evacuate addr)
        i@PrimInt{} -> pure i )

    updateClosure :: MemAddr -> Closure -> Gc ()
    updateClosure addr closure = do
        gcState@GcState { toHeap = heap } <- getGcState
        let heap' = H.update (Mapping addr (HClosure closure)) heap
        putGcState gcState { toHeap = heap' }

    registerForEvacuation :: [MemAddr] -> Gc ()
    registerForEvacuation addresses = do
        gcState@GcState { toEvacuate = evac } <- getGcState
        putGcState gcState { toEvacuate = evac <> Seq.fromList addresses }
