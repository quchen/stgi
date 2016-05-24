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
import           Data.Set                   (Set)
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

newtype Gc a = Gc (ReaderT (Old Heap) (State GcState) a)
    deriving (Functor, Applicative, Monad)

askOldHeap :: Gc (Old Heap)
askOldHeap = Gc ask

getGcState :: Gc GcState
getGcState = Gc (lift get)

putGcState :: GcState -> Gc ()
putGcState s = Gc (lift (put s))

execGc :: Gc a -> Old Heap -> GcState -> GcState
execGc (Gc rsa) oldHeap gcState =
    let sa = runReaderT rsa oldHeap
        finalState = execState sa gcState
    in finalState

splitHeap :: StgState -> (Dead Heap, Alive Heap)
splitHeap StgState
    { stgCode    = code
    , stgHeap    = heap
    , stgGlobals = globals
    , stgStack   = stack }
  = let rootAddrs = mconcat [addrs code, addrs globals, addrs stack]
        initialState = GcState
            { aliveHeap = mempty
            , forwards = mempty
            , toEvacuate = Alive rootAddrs }
        finalState = execGc evacuateScavenge (Old heap) initialState
    in case finalState of
        GcState {aliveHeap = alive} ->
            let Alive (Heap a) = alive
                Heap o         = heap
                dead           = Dead (Heap (o `M.difference` a))
            in (dead, alive)

evacuateScavenge :: Gc ()
evacuateScavenge = do
    GcState { toEvacuate = Alive evacuateNext } <- getGcState
    unless (S.null evacuateNext) (do
        Old heap <- askOldHeap
        toAddrs <- for (toList evacuateNext) evacuate
        evacuateNext' <- fmap mconcat ( for toAddrs (\toAddr -> case H.lookup toAddr heap of
            Nothing -> error "evacuateScavenge error: address not found!"
            Just heapObject -> scavenge heapObject ))
        gcState <- getGcState
        putGcState gcState { toEvacuate = evacuateNext' }
        evacuateScavenge )

data GcState = GcState
    { aliveHeap :: Alive Heap
        -- ^ Heap of closures known to be alive.
        --   Has no overlap with the old heap.

    , forwards :: Map MemAddr MemAddr
        -- ^ Forward pointers to the new locations of already collected heap
        -- objects

    , toEvacuate :: Alive (Set MemAddr)
        -- ^ Closures known to be alive, but not yet evacuated
    } deriving (Eq, Ord, Show)

followForwardChain :: MemAddr -> Map MemAddr MemAddr -> MemAddr
followForwardChain addr forw = case M.lookup addr forw of
    Just addr' -> followForwardChain addr' forw
    Nothing    -> addr

-- | Copy a closure from from-space to to-space, and return the new memory
-- address.
evacuate :: MemAddr -> Gc MemAddr
evacuate addr = do
    gcState@GcState
        { aliveHeap = Alive alive
        , forwards  = forw } <- getGcState
    let alreadyEvacuated = M.lookup addr forw
    case alreadyEvacuated of
        Just forward -> pure forward
        Nothing -> do
            Old heap <- askOldHeap
            case H.lookup (followForwardChain addr forw) heap of
                Nothing -> error "Tried collecting a non-existent memory address!"
                Just heapObject -> do
                    let (addr', alive') = H.alloc heapObject alive
                        forwards' = M.insert addr addr' forw
                    putGcState gcState { aliveHeap = Alive alive'
                                       , forwards = forwards' }
                    pure addr'

scavenge :: HeapObject -> Gc (Alive (Set MemAddr))
scavenge Blackhole{} = pure mempty
scavenge (HClosure (Closure _lf frees)) =
    fmap (Alive . S.fromList) (sequence [ evacuate addr | Addr addr <- frees ])
