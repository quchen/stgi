{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

-- | Remove unused heap objects.
module Stg.Machine.GarbageCollection (
    garbageCollect,

    -- * Algorithms
    GarbageCollectionAlgorithm,
    triStateTracing,
    copying,
) where



import Data.Foldable
import Data.Tagged

import           Stg.Machine.GarbageCollection.Common
import           Stg.Machine.GarbageCollection.Copying
import           Stg.Machine.GarbageCollection.TriStateTracing
import qualified Stg.Machine.Heap                              as H
import           Stg.Machine.Types



garbageCollect :: GarbageCollectionAlgorithm -> StgState -> StgState
garbageCollect algorithm state
  = let (Tagged deadHeap, state') = splitHeapWith algorithm state
    in if H.size deadHeap > 0
        then state' { stgSteps = stgSteps state + 1
                    , stgInfo  = Info GarbageCollection
                                      [Detail_GarbageCollected (toList (H.addresses deadHeap))] }
        else state
