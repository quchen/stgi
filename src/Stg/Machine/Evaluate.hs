{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Evaluate STG 'Program's.
module Stg.Machine.Evaluate (
    evalStep,
) where



import Data.Foldable

import qualified Data.Stack                            as S
import qualified Stg.Machine.Evaluate.ErrorTransitions as Error
import qualified Stg.Machine.Evaluate.ValidTransitions as Valid
import           Stg.Machine.Types



-- | Perform a single STG machine evaluation step.
evalStep :: StgState -> StgState
evalStep state = let state' = stgRule state
                 in state' { stgSteps = stgSteps state' + 1 }



-- | Transition rules detailed in the 1992 paper, along with error rules to
-- help if none of them applies.
rules :: [StgState -> Maybe StgState]
rules =
    [ Valid.rule1
    , Valid.rule2
    , Valid.rule3
    , Valid.rule1819
    , Valid.rule4
    , Valid.rule5
    , Valid.rule6
    , Valid.rule7
    , Valid.rule8
    , Valid.rule9
    , Valid.rule10
    , Valid.rule11
    , Valid.rule12
    , Valid.rule13
    , Valid.rule14
    , Valid.rule15
    , Valid.rule16
    , Valid.rule17a

    , Error.updatableClosureWithArgs
    , Error.returnWithEmptyReturnStack
    , Error.functionArgumentNotInScope
    , Error.constructorArgumentNotInScope
    , Error.primopArgumentNotInScope
    , Error.algReturnToPrimAlts
    , Error.primReturnToAlgAlts
    , Error.enterBlackhole
    , Error.updateClosureWithPrimitive
    , Error.nonAlgPrimScrutinee
    , Error.divisionByZero
    , Error.badConArity ]



-- | Apply a single applicable STG evaluation rule to continue to the next step.
stgRule :: StgState -> StgState
stgRule state = case asum [ rule state | rule <- rules ] of
    Nothing     -> noRulesApply state
    Just state' -> state'



-- | Fallback if none of the known rules applies.
noRulesApply :: StgState -> StgState
noRulesApply s = s { stgInfo = Info NoRulesApply detail }
  where
    detail = case stgStack s of
        S.Empty -> []
        _else   -> [Detail_StackNotEmpty]
