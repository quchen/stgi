{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

-- | Mark-and-sweep garbage collector.
module Stg.Machine.GarbageCollection.MarkAndSweep (
    markAndSweep,
) where



import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Monoid hiding (Alt)
import           Data.Set    (Set)
import qualified Data.Set    as S

import Stg.Machine.GarbageCollection.Common
import Stg.Machine.Types



markAndSweep :: GarbageCollectionAlgorithm
markAndSweep = GarbageCollectionAlgorithm splitHeap

-- | Split the heap of an 'StgState' in two components: dead (can be discarded)
-- and alive (are still used) closures.
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
            , evacuate = (Alive . mconcat)
                [addrs code, addrs globals, addrs stack] }
    in (Dead dead, alive)

everythingCollected :: GcState -> Bool
everythingCollected = noAlives
  where
    noAlives GcState {evacuate = Alive alive} = S.null alive

data GcState = GcState
    { aliveHeap :: Alive Heap
        -- ^ Heap of closures known to be alive.
        --   Has no overlap with the old heap.

    , evacuate :: Alive (Set MemAddr)
        -- ^ Memory addresses known to be alive,
        --   but not yet scavenged from the old heap.

    , oldHeap :: Heap
        -- ^ The old heap, containing both dead
        --   and not-yet-found alive closures.
    } deriving (Eq, Ord, Show)

gcStep :: GcState -> GcState
gcStep GcState
    { aliveHeap = oldAlive@(Alive (Heap alive))
    , evacuate  = Alive evacuateAddrs
    , oldHeap   = Heap oldRest }
  = GcState
    { aliveHeap = oldAlive <> Alive (Heap scavenged)
    , evacuate  = Alive (addrs scavenged)
    , oldHeap   = Heap newRest }
  where
    scavenged, newRest :: Map MemAddr HeapObject
    (scavenged, newRest) = M.partitionWithKey isAlive oldRest
      where
        -- A closure is alive iff it is on the alive heap, or the closure that
        -- contained it was scavenged in a previous step.
        isAlive addr _closure = M.member addr alive
                             || S.member addr evacuateAddrs
