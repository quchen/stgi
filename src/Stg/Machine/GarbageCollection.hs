{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

-- | Remove unused heap objects.
module Stg.Machine.GarbageCollection (
    garbageCollect,

    -- * Algorithms
    GarbageCollectionAlgorithm,
    triStateTracing,
    twoSpaceCopying,
) where



import qualified Data.Set as S

import Stg.Machine.GarbageCollection.Common
import Stg.Machine.GarbageCollection.TriStateTracing
import Stg.Machine.GarbageCollection.TwoSpaceCopying
import Stg.Machine.Types



garbageCollect :: GarbageCollectionAlgorithm -> StgState -> StgState
garbageCollect algorithm@(GarbageCollectionAlgorithm name _) state
  = let (deadAddrs, forwards, state') = splitHeapWith algorithm state
    in if S.size deadAddrs > 0
        then state' { stgSteps = stgSteps state + 1
                    , stgInfo  = Info GarbageCollection
                                      [Detail_GarbageCollected name deadAddrs forwards] }
        else state
