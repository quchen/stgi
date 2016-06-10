{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}

-- | Two space stop-and-copy garbage collector.
--
-- * Compacting: memory is moved to a new location
-- * Tracing
module Stg.Machine.GarbageCollection.Copying (
    copying,
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
import qualified Data.Set                   as S
import           Data.Tagged
import           Data.Traversable

import           Data.Stack                           (Stack)
import           Stg.Machine.GarbageCollection.Common
import qualified Stg.Machine.Heap                     as H
import           Stg.Machine.Types



-- | Remove all unused addresses by moving them to a safe location.
copying :: GarbageCollectionAlgorithm
copying = GarbageCollectionAlgorithm splitHeap

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
        -- only objects that are also in the 'toHeap'.

    , toEvacuate :: Seq MemAddr
        -- ^ Heap objects known to be alive, but not yet evacuated.
    } deriving (Eq, Ord, Show)

splitHeap :: StgState -> (Tagged Dead Heap, StgState)
splitHeap stgState@StgState
    { stgCode    = code
    , stgHeap    = heap
    , stgGlobals = globals
    , stgStack   = stack }
  = let rootAddrs = (mconcat . map (Seq.fromList . S.toList))
                        [addrs code, addrs globals, addrs stack]
        initialState = GcState
            { toHeap     = mempty
            , forwards   = mempty
            , toScavenge = mempty
            , toEvacuate = rootAddrs }
        finalState = execGc evacuateScavengeLoop heap initialState
    in case finalState of
        GcState {toHeap = heap'@(Heap alive'), forwards = forwards'} ->
            let Heap old = heap
                dead     = Heap (old `M.difference` alive')

                forward addr = M.findWithDefault forwardErr addr forwards'
                forwardErr = error "Invalid forward in GC; please report this as a bug"

                stgState' = stgState
                    { stgCode  = updateAddrs forward code
                    , stgStack = updateAddrs forward stack
                    , stgHeap  = heap' }
            in (Tagged dead, stgState')

evacuateScavengeLoop :: Gc ()
evacuateScavengeLoop = initialEvacuation >> scavengeLoop

initialEvacuation :: Gc ()
initialEvacuation = do
    GcState { toEvacuate = evacuateNext } <- getGcState
    traverse_ evacuate evacuateNext

scavengeLoop :: Gc ()
scavengeLoop = do
    GcState { toScavenge = scavengeNext } <- getGcState
    if | Seq.null scavengeNext -> pure ()
       | otherwise -> scavengeAll scavengeNext >> scavengeLoop

scavengeAll :: Seq MemAddr -> Gc ()
scavengeAll = go S.empty
  where
    go cache toAddrs = case Seq.viewl toAddrs of
        EmptyL -> pure ()
        addr :< rest
            | S.member addr cache -> go cache rest
            | otherwise -> do
                scavenge addr
                go (S.insert addr cache) rest


data EvacuationStatus = NotEvacuated | AlreadyEvacuated MemAddr

-- | Copy a closure from from-space to to-space, and return the new memory
-- address.
--
-- If the closure has previously been evacuated do nothing, and return only the
-- new address.
evacuate :: MemAddr -> Gc MemAddr
evacuate = \fromAddr -> resolveForward fromAddr >>= \case
    AlreadyEvacuated newAddr -> pure newAddr
    NotEvacuated -> fmap (H.lookup fromAddr) askHeap >>= \case
        Nothing -> error "Tried collecting a non-existent memory address!\
                         \ Please report this as a bug."
        Just heapObject -> do

            -- 1. Copy object into to-space
            newAddr <- do
                GcState { toHeap = to } <- getGcState
                let (addr', to') = H.alloc heapObject to
                setToHeap to'
                pure addr'

            -- 2. Register the object to be scavenged
            do  gcState@GcState { toScavenge = sc } <- getGcState
                putGcState gcState { toScavenge = sc |> newAddr }

            -- 2. Create forwarding pointer to make evacuation idempotent
            createForward fromAddr newAddr

            -- 3. Return new to-space address
            pure newAddr
  where
    resolveForward :: MemAddr -> Gc EvacuationStatus
    resolveForward addr = do
        GcState { forwards = forw } <- getGcState
        pure (case M.lookup addr forw of
            Nothing -> NotEvacuated
            Just newAddr -> AlreadyEvacuated newAddr )

    setToHeap :: Heap -> Gc ()
    setToHeap to = do
        gcState <- getGcState
        putGcState gcState { toHeap = to }

    createForward :: MemAddr -> MemAddr -> Gc ()
    createForward from to = do
        gcState@GcState{forwards = forw} <- getGcState
        putGcState gcState { forwards = M.insert from to forw }

-- | Find referenced addresses in a heap object, and overwrite them with their
-- evacuated new addresses.
scavenge :: MemAddr -> Gc ()
scavenge scavengeAddr = do
    scavengeHeapObject <- do
        GcState { toHeap = heap } <- getGcState
        pure (H.lookup scavengeAddr heap)
    case scavengeHeapObject of
        Nothing -> error "Scavenge error: address not found on to-heap\
                         \ Please report this as a bug."
        Just Blackhole{} -> pure mempty
        Just (HClosure (Closure lf frees)) -> do

            -- 1. Call the evacuation code of all contained addresses
            frees' <- for frees (\case
                Addr addr -> fmap (\x -> Addr x) (evacuate addr)
                i@PrimInt{} -> pure i )

            -- 2. Replace the pointers in the original closure with their
            --    new to-space addresses
            do  gcState@GcState { toHeap = heap } <- getGcState
                let closure' = HClosure (Closure lf frees')
                    heap' = H.update scavengeAddr closure' heap
                putGcState gcState { toHeap = heap' }

            -- 3. Add the addresses found in the just evacuated heap objects
            --    to the to-reclaim list
            do  gcState@GcState { toEvacuate = evac } <- getGcState
                let newEvacs = Seq.fromList [ addr | Addr addr <- frees' ]
                putGcState gcState { toEvacuate = evac <> newEvacs }

class UpdateAddrs a where
    updateAddrs :: (MemAddr -> MemAddr) -> a -> a

instance UpdateAddrs Code where
    updateAddrs upd = \case
        Eval expr locals      -> Eval expr (updateAddrs upd locals)
        Enter addr            -> Enter (updateAddrs upd addr)
        ReturnCon constr args -> ReturnCon constr (updateAddrs upd args)
        r@ReturnInt{}         -> r

instance UpdateAddrs Locals where
    updateAddrs upd (Locals locals) = Locals (M.map (updateAddrs upd) locals)

instance UpdateAddrs Value where
    updateAddrs upd = \case
        Addr addr   -> Addr (updateAddrs upd addr)
        p@PrimInt{} -> p

instance UpdateAddrs MemAddr where
    updateAddrs = id

instance UpdateAddrs a => UpdateAddrs [a] where
    updateAddrs upd = map (updateAddrs upd)

instance UpdateAddrs a => UpdateAddrs (Stack a) where
    updateAddrs upd = fmap (updateAddrs upd)

instance UpdateAddrs StackFrame where
    updateAddrs upd = \case
        ArgumentFrame arg       -> ArgumentFrame (updateAddrs upd arg)
        ReturnFrame alts locals -> ReturnFrame alts (updateAddrs upd locals)
        UpdateFrame addr        -> UpdateFrame (updateAddrs upd addr)
