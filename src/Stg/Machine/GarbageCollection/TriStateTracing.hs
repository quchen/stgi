{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

-- | Tri-state ("tri-colour") tracing garbage collector.
module Stg.Machine.GarbageCollection.TriStateTracing (
    triStateTracing,
) where



import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Monoid hiding (Alt)
import           Data.Set    (Set)
import qualified Data.Set    as S

import Stg.Machine.GarbageCollection.Common
import Stg.Machine.Types



-- | Remove all unused addresses, without moving the others.
triStateTracing :: GarbageCollectionAlgorithm
triStateTracing = GarbageCollectionAlgorithm splitHeap

splitHeap :: StgState -> (Dead Heap, Alive Heap)
splitHeap StgState
    { stgCode    = code
    , stgHeap    = heap
    , stgGlobals = globals
    , stgStack   = stack }
  = let GcState {aliveHeap = alive, oldHeap = dead}
            = until everythingCollected gcStep start
        start = GcState
            { aliveHeap = mempty
            , oldHeap = heap
            , staged = (Alive . mconcat)
                [addrs code, addrs globals, addrs stack] }
    in (Dead dead, alive)

everythingCollected :: GcState -> Bool
everythingCollected = noAlives
  where
    noAlives GcState {staged = Alive alive} = S.null alive

-- | Each closure is in one of three states: in the alive heap, staged for
-- later rescue, or not even staged yet.
data GcState = GcState
    { aliveHeap :: Alive Heap
        -- ^ Heap of closures known to be alive.
        --   Has no overlap with the old heap.

    , staged :: Alive (Set MemAddr)
        -- ^ Memory addresses known to be alive,
        --   but not yet rescued from the old heap.

    , oldHeap :: Heap
        -- ^ The old heap, containing both dead
        --   and not-yet-found alive closures.
    } deriving (Eq, Ord, Show)

gcStep :: GcState -> GcState
gcStep GcState
    { aliveHeap = oldAlive@(Alive (Heap alive))
    , staged    = Alive stagedAddrs
    , oldHeap   = Heap oldRest }
  = GcState
    { aliveHeap = oldAlive <> Alive (Heap rescued)
    , staged    = Alive (addrs rescued)
    , oldHeap   = Heap newRest }
  where
    rescued, newRest :: Map MemAddr HeapObject
    (rescued, newRest) = M.partitionWithKey isAlive oldRest
      where
        isAlive addr _closure = M.member addr alive
                             || S.member addr stagedAddrs
