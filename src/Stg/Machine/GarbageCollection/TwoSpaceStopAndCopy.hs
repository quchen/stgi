{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

-- | Two space stop-and-copy garbage collector.
--
-- * Compacting: Alive memory is moved to a new location
-- * Tracing
module Stg.Machine.GarbageCollection.TwoSpaceStopAndCopy (
    twoSpaceStopAndCopy,
) where



import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Monoid
import qualified Data.Set                   as S
import           Data.Traversable

import           Stg.Machine.GarbageCollection.Common
import qualified Stg.Machine.Heap                     as H
import           Stg.Machine.Types



-- | Remove all unused addresses by moving them to a safe location.
twoSpaceStopAndCopy :: GarbageCollectionAlgorithm
twoSpaceStopAndCopy = GarbageCollectionAlgorithm splitHeap

-- | Old objects represent the previous generation of alive objects, and
-- contain both dead and alive objects.
newtype Old a = Old a
    deriving (Eq, Ord, Show, Monoid)

-- | Wrapper tag to specify a value is in "from" space.
newtype From a = From a deriving (Eq, Ord, Show, Monoid)

-- | Wrapper tag to specify a value is in "to" space.
newtype To a = To a deriving (Eq, Ord, Show, Monoid)

newtype Gc a = Gc (ReaderT (From Heap) (State GcState) a)
    deriving (Functor, Applicative, Monad)

askFromHeap :: Gc (From Heap)
askFromHeap = Gc ask

getGcState :: Gc GcState
getGcState = Gc (lift get)

putGcState :: GcState -> Gc ()
putGcState s = Gc (lift (put s))

execGc :: Gc a -> From Heap -> GcState -> GcState
execGc (Gc rsa) oldHeap gcState =
    let sa = runReaderT rsa oldHeap
        finalState = execState sa gcState
    in finalState

data GcState = GcState
    { toHeap :: To Heap
        -- ^ Heap of closures known to be alive.

    , forwards :: Map (From MemAddr) (To MemAddr)
        -- ^ Forward pointers to the new locations of already collected heap
        -- objects

    , toEvacuate :: Alive [From MemAddr]
        -- ^ Closures known to be alive, but not yet evacuated.
    } deriving (Eq, Ord, Show)

splitHeap :: StgState -> (Dead Heap, Alive Heap)
splitHeap StgState
    { stgCode    = code
    , stgHeap    = heap
    , stgGlobals = globals
    , stgStack   = stack }
  = let rootAddrs = (map From . concatMap S.toList) [addrs code, addrs globals, addrs stack]
        initialState = GcState
            { toHeap = mempty
            , forwards = mempty
            , toEvacuate = Alive rootAddrs }
        finalState = execGc evacuateScavengeLoop (From heap) initialState
    in case finalState of
        GcState {toHeap = To (Heap alive')} ->
            let Heap old = heap
                dead     = Dead (Heap (old `M.difference` alive'))
                alive    = Alive (Heap alive')
            in (dead, alive)

evacuateScavengeLoop :: Gc ()
evacuateScavengeLoop = do
    gcState@GcState { toEvacuate = Alive evacuateNext } <- getGcState
    unless (null evacuateNext) (do
        putGcState gcState { toEvacuate = mempty }

        -- NB: Unfortunately we cannot fuse these traversals, since scavenging
        --     assumes all heap objects have been evacuated
        traverse evacuate evacuateNext >>= traverse_ scavenge

        evacuateScavengeLoop )



data EvacuationStatus = NotEvacuated | AlreadyEvacuated (To MemAddr)

-- | Copy a closure from from-space to to-space, and return the new memory
-- address.
--
-- If the closure has previously been evacuated do nothing, and return only the
-- new address.
evacuate :: From MemAddr -> Gc (To MemAddr)
evacuate = \fromAddr -> resolveForward fromAddr >>= \case
    AlreadyEvacuated newAddr -> pure newAddr
    NotEvacuated -> do
        valueOnOldHeap <- do
            From heap <- askFromHeap
            let From addr = fromAddr
            pure (H.lookup addr heap)
        case valueOnOldHeap of
            Nothing -> error "Tried collecting a non-existent memory address!\
                             \ Please report this as a bug."
            Just heapObject -> do

                -- 1. Copy object into to-space
                newAddr <- do
                    GcState { toHeap = To to } <- getGcState
                    let (addr', to') = H.alloc heapObject to
                    setAliveHeap (To to')
                    pure (To addr')

                -- 2. Create forwarding pointer to make evacuation idempotent
                createForward fromAddr newAddr

                -- 3. Return new to-space address
                pure newAddr
  where
    resolveForward :: From MemAddr -> Gc EvacuationStatus
    resolveForward addr = do
        GcState { forwards = forw } <- getGcState
        pure (case M.lookup addr forw of
            Nothing -> NotEvacuated
            Just newAddr -> AlreadyEvacuated newAddr )

    setAliveHeap :: To Heap -> Gc ()
    setAliveHeap to = do
        gcState <- getGcState
        putGcState gcState { toHeap = to }

    createForward :: From MemAddr -> To MemAddr -> Gc ()
    createForward from to = do
        gcState@GcState{forwards = forw} <- getGcState
        putGcState gcState { forwards = M.insert from to forw }

-- | Find referenced addresses in a heap object, and overwrite them with their
-- evacuated new addresses.
scavenge :: To MemAddr -> Gc ()
scavenge (To scavengeAddr) = do
    scavengeHeapObject <- do
        GcState { toHeap = To heap } <- getGcState
        pure (H.lookup scavengeAddr heap)
    case scavengeHeapObject of
        Nothing -> error "Scavenge error: address not found on to-heap\
                         \ Please report this as a bug."
        Just Blackhole{} -> pure mempty
        Just (HClosure (Closure lf frees)) -> do

            -- 1. Call the evacuation code of all contained addresses
            frees' <- for frees (\case
                Addr addr -> fmap (\(To x) -> Addr x) (evacuate (From addr))
                i@PrimInt{} -> pure i )

            -- 2. Replace the pointers in the original closure with their
            --    new to-space addresses
            do  gcState@GcState { toHeap = To heap } <- getGcState
                let closure' = HClosure (Closure lf frees')
                    heap' = H.update scavengeAddr closure' heap
                putGcState gcState { toHeap = To heap' }

            -- 3. Add the addresses found in the just evacuated heap objects
            --    to the to-reclaim list
            do  gcState@GcState { toEvacuate = evac } <- getGcState
                let newEvacs = Alive [ From addr | Addr addr <- frees' ]
                putGcState gcState { toEvacuate = evac <> newEvacs }
