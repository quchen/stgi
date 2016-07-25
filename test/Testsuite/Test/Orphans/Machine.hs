{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Orphans.Machine () where



import qualified Data.Map    as M
import           Data.Monoid
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
    arbitrary = arbitrary1 MemAddr

instance Arbitrary StackFrame where
    arbitrary = oneof [ arbitrary1 ArgumentFrame
                      , arbitrary2 ReturnFrame
                      , arbitrary1 UpdateFrame ]

instance Arbitrary Value where
    arbitrary = oneof [ arbitrary1 Addr
                      , arbitrary1 PrimInt ]

instance Arbitrary Code where
    arbitrary = oneof [ arbitrary2 Eval
                      , arbitrary1 Enter
                      , arbitrary2 ReturnCon
                      , arbitrary1 ReturnInt ]

instance (Arbitrary k, Arbitrary v) => Arbitrary (Mapping k v) where
    arbitrary = arbitrary2 Mapping

instance Arbitrary Globals where
    arbitrary = arbitrary1 (Globals . M.fromList)

instance Arbitrary Locals where
    arbitrary = arbitrary1 (Locals . M.fromList)

instance Arbitrary Closure where
    arbitrary = arbitrary2 Closure

instance Arbitrary Heap where
    arbitrary = arbitrary1 (Heap . M.fromList)

instance Arbitrary HeapObject where
    arbitrary = oneof [ arbitrary1 HClosure
                      , arbitrary1 Blackhole ]

instance Arbitrary Info where
    arbitrary = arbitrary2 Info

instance Arbitrary InfoShort where
    arbitrary = oneof [ pure NoRulesApply
                      , pure MaxStepsExceeded
                      , pure HaltedByPredicate
                      , arbitrary1 StateTransition
                      , arbitrary1 StateError
                      , pure StateInitial ]

instance Arbitrary InfoDetail where
    arbitrary = oneof
        [ arbitrary2 Detail_FunctionApplication
        , arbitrary2 Detail_UnusedLocalVariables
        , arbitrary2 Detail_EnterNonUpdatable
        , arbitrary2 Detail_EvalLet
        , arbitrary0 Detail_EvalCase
        , arbitrary1 Detail_EnterUpdatable
        , arbitrary2 Detail_ConUpdate
        , arbitrary1 Detail_PapUpdate
        , arbitrary0 Detail_ReturnIntCannotUpdate
        , arbitrary0 Detail_StackNotEmpty
        , Detail_GarbageCollected
            <$> ((\x -> "GC algorithm: " <> T.pack x) <$> arbitrary)
            <*> (S.fromList <$> arbitrary)
            <*> (M.fromList <$> arbitrary)
        , arbitrary2 Detail_EnterBlackHole ]

instance Arbitrary StateTransition where
    arbitrary = oneof
        [ arbitrary0 Rule1_Eval_FunctionApplication
        , arbitrary0 Rule2_Enter_NonUpdatableClosure
        , arbitrary1 Rule3_Eval_Let
        , arbitrary0 Rule4_Eval_Case
        , arbitrary0 Rule5_Eval_AppC
        , arbitrary0 Rule6_ReturnCon_Match
        , arbitrary0 Rule7_ReturnCon_DefUnbound
        , arbitrary0 Rule8_ReturnCon_DefBound
        , arbitrary0 Rule9_Lit
        , arbitrary0 Rule10_LitApp
        , arbitrary0 Rule11_ReturnInt_Match
        , arbitrary0 Rule12_ReturnInt_DefBound
        , arbitrary0 Rule13_ReturnInt_DefUnbound
        , arbitrary0 Rule14_Eval_AppP
        , arbitrary0 Rule15_Enter_UpdatableClosure
        , arbitrary0 Rule16_ReturnCon_Update
        , arbitrary0 Rule17a_Enter_PartiallyAppliedUpdate
        , arbitrary0 Rule1819_Eval_Case_Primop_Shortcut ]

instance Arbitrary StateError where
    arbitrary = oneof
        [ arbitrary1 VariablesNotInScope
        , arbitrary0 UpdatableClosureWithArgs
        , arbitrary0 ReturnIntWithEmptyReturnStack
        , arbitrary0 AlgReturnToPrimAlts
        , arbitrary0 PrimReturnToAlgAlts
        , arbitrary0 InitialStateCreationFailed
        , arbitrary0 EnterBlackhole ]

instance Arbitrary NotInScope where
    arbitrary = arbitrary1 NotInScope
