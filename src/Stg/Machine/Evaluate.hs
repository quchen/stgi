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
--
-- This is the place to modify the ruleset of the machine, for example
-- 'Valid.rule1819' can be removed to yield a less efficient, yet equally
-- correct, STG implementation.
rules :: [StgState -> Maybe StgState]
rules =
    [ Valid.rule1_functionApp
    , Valid.rule2_enterNonUpdatable
    , Valid.rule3_let
    , Valid.rule1819_casePrimopShortcut -- before rule 4 because it takes
                                        -- precedence over it in certain cases!
    , Valid.rule4_case
    , Valid.rule5_constructorApp
    , Valid.rule6_algebraicNormalMatch
    , Valid.rule7_algebraicUnboundDefaultMatch
    , Valid.rule8_algebraicBoundDefaultMatch
    , Valid.rule9_primitiveLiteralEval
    , Valid.rule10_primitiveLiteralApp
    , Valid.rule11_primitiveNormalMatch
    , Valid.rule12_primitiveBoundDefaultMatch
    , Valid.rule13_primitiveUnboundDefaultMatch
    , Valid.rule14_primop
    , Valid.rule15_enterUpdatable
    , Valid.rule16_missingReturnUpdate
    , Valid.rule17a_missingArgUpdate

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
