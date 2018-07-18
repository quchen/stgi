{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Orphans.Machine () where



import qualified Data.Map    as M
import qualified Data.Set    as S
import qualified Data.Text   as T

import Stg.Machine.Types

import Test.Orphans.Language ()
import Test.Orphans.Stack    ()
import Test.Tasty.QuickCheck
import Test.Util



instance Arbitrary StgState where
    arbitrary = StgState
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary MemAddr where
    arbitrary = arbitrary_1 MemAddr

instance Arbitrary StackFrame where
    arbitrary = oneof [ arbitrary_1 ArgumentFrame
                      , arbitrary_2 ReturnFrame
                      , arbitrary_1 UpdateFrame ]

instance Arbitrary Value where
    arbitrary = oneof [ arbitrary_1 Addr
                      , arbitrary_1 PrimInt ]

instance Arbitrary Code where
    arbitrary = oneof [ arbitrary_2 Eval
                      , arbitrary_1 Enter
                      , arbitrary_2 ReturnCon
                      , arbitrary_1 ReturnInt ]

instance (Arbitrary k, Arbitrary v) => Arbitrary (Mapping k v) where
    arbitrary = arbitrary_2 Mapping

instance Arbitrary Globals where
    arbitrary = arbitrary_1 (Globals . M.fromList)

instance Arbitrary Locals where
    arbitrary = arbitrary_1 (Locals . M.fromList)

instance Arbitrary Closure where
    arbitrary = arbitrary_2 Closure

instance Arbitrary Heap where
    arbitrary = arbitrary_1 (Heap . M.fromList)

instance Arbitrary HeapObject where
    arbitrary = oneof [ arbitrary_1 HClosure
                      , arbitrary_1 Blackhole ]

instance Arbitrary Info where
    arbitrary = arbitrary_2 Info

instance Arbitrary InfoShort where
    arbitrary = oneof [ pure NoRulesApply
                      , pure MaxStepsExceeded
                      , pure HaltedByPredicate
                      , arbitrary_1 StateTransition
                      , arbitrary_1 StateError
                      , pure StateInitial ]

instance Arbitrary InfoDetail where
    arbitrary = oneof
        [ arbitrary_2 Detail_FunctionApplication
        , arbitrary_2 Detail_UnusedLocalVariables
        , arbitrary_2 Detail_EnterNonUpdatable
        , arbitrary_2 Detail_EvalLet
        , arbitrary_0 Detail_EvalCase
        , arbitrary_1 Detail_EnterUpdatable
        , arbitrary_2 Detail_ConUpdate
        , arbitrary_1 Detail_PapUpdate
        , arbitrary_0 Detail_ReturnIntCannotUpdate
        , arbitrary_0 Detail_StackNotEmpty
        , Detail_GarbageCollected
            <$> ((\x -> "GC algorithm: " <> T.pack x) <$> arbitrary)
            <*> (S.fromList <$> arbitrary)
            <*> (M.fromList <$> arbitrary)
        , arbitrary_2 Detail_EnterBlackHole ]

instance Arbitrary StateTransition where
    arbitrary = oneof
        [ arbitrary_0 Rule1_Eval_FunctionApplication
        , arbitrary_0 Rule2_Enter_NonUpdatableClosure
        , arbitrary_1 Rule3_Eval_Let
        , arbitrary_0 Rule4_Eval_Case
        , arbitrary_0 Rule5_Eval_AppC
        , arbitrary_0 Rule6_ReturnCon_Match
        , arbitrary_0 Rule7_ReturnCon_DefUnbound
        , arbitrary_0 Rule8_ReturnCon_DefBound
        , arbitrary_0 Rule9_Lit
        , arbitrary_0 Rule10_LitApp
        , arbitrary_0 Rule11_ReturnInt_Match
        , arbitrary_0 Rule12_ReturnInt_DefBound
        , arbitrary_0 Rule13_ReturnInt_DefUnbound
        , arbitrary_0 Rule14_Eval_AppP
        , arbitrary_0 Rule15_Enter_UpdatableClosure
        , arbitrary_0 Rule16_ReturnCon_Update
        , arbitrary_0 Rule17a_Enter_PartiallyAppliedUpdate
        , arbitrary_0 Rule1819_Eval_Case_Primop_Shortcut ]

instance Arbitrary StateError where
    arbitrary = oneof
        [ arbitrary_1 VariablesNotInScope
        , arbitrary_0 UpdatableClosureWithArgs
        , arbitrary_0 ReturnIntWithEmptyReturnStack
        , arbitrary_0 AlgReturnToPrimAlts
        , arbitrary_0 PrimReturnToAlgAlts
        , arbitrary_0 InitialStateCreationFailed
        , arbitrary_0 EnterBlackhole ]

instance Arbitrary NotInScope where
    arbitrary = arbitrary_1 NotInScope
