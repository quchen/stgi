{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

-- | Remove unused heap objects.
module Stg.Machine.GarbageCollection (
    garbageCollect,

    -- * Algorithms
    GarbageCollectionAlgorithm,
    markAndSweep,
    twoSpaceStopAndCopy,
) where



import Data.Foldable

import           Stg.Machine.GarbageCollection.Common
import           Stg.Machine.GarbageCollection.MarkAndSweep
import           Stg.Machine.GarbageCollection.TwoSpaceStopAndCopy
import qualified Stg.Machine.Heap                                  as H
import           Stg.Machine.Types



garbageCollect :: GarbageCollectionAlgorithm -> StgState -> StgState
garbageCollect algorithm state
  = let (Dead deadHeap, Alive cleanHeap) = splitHeapWith algorithm state
    in if H.size deadHeap > 0
        then state { stgHeap  = cleanHeap
                   , stgSteps = stgSteps state + 1
                   , stgInfo  = Info GarbageCollection
                                     [Detail_GarbageCollected (toList (H.addresses deadHeap))] }
        else state
