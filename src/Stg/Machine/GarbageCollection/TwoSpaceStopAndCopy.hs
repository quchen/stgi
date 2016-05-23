{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

-- | Two space stop-and-copy garbage collector.
module Stg.Machine.GarbageCollection.TwoSpaceStopAndCopy (
    twoSpaceStopAndCopy,
) where



import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Set                  (Set)
import qualified Data.Set                  as S
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

splitHeap :: StgState -> (Dead Heap, Alive Heap)
splitHeap StgState
    { stgCode    = code
    , stgHeap    = heap
    , stgGlobals = globals
    , stgStack   = stack }
  = let GcState {aliveHeap = alive}
            = execState (evacuateScavenge (Old heap)) GcState
                { aliveHeap = mempty
                , forwards = mempty
                , toEvacuate = (Alive . mconcat)
                    [addrs code, addrs globals, addrs stack] }
        dead = let Alive (Heap a) = alive
                   (Heap o)       = heap
               in Dead (Heap (o `M.difference` a))
    in (dead, alive)

evacuateScavenge :: Old Heap -> State GcState ()
evacuateScavenge oldHeap@(Old heap) = do
    GcState { toEvacuate = Alive evacuateNext } <- get
    unless (S.null evacuateNext) (do
        toAddrs <- for (toList evacuateNext) (evacuate oldHeap)
        for_ toAddrs (\toAddr -> case H.lookup toAddr heap of
            Nothing -> error "evacuateScavenge error: address not found!"
            Just heapObject -> scavenge oldHeap heapObject )
        evacuateScavenge oldHeap )

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
evacuate :: Old Heap -> MemAddr -> State GcState MemAddr
evacuate (Old heap) addr = do
    gcState@GcState
        { aliveHeap = Alive alive
        , forwards  = forw } <- get

    let alreadyCollected = M.lookup addr forw
    case alreadyCollected of
        Nothing -> case H.lookup (followForwardChain addr forw) heap of
            Nothing -> error "Tried collecting a non-existent memory address!"
            Just heapObject -> do
                let (addr', alive') = H.alloc heapObject alive
                    forwards' = M.insert addr addr' forw
                put gcState { aliveHeap = Alive alive'
                            , forwards = forwards' }
                pure addr'

        -- Forward pointer exists, so the object has previously been collected.
        -- Don't do anything to it.
        Just forward -> pure forward

scavenge :: Old Heap -> HeapObject -> State GcState HeapObject
scavenge _ bh@Blackhole{} = pure bh
scavenge oldHeap (HClosure (Closure lf frees)) = do
    frees' <- for frees (\case
        Addr addr -> fmap Addr (evacuate oldHeap addr )
        PrimInt i -> pure (PrimInt i) )
    pure (HClosure (Closure lf frees'))
