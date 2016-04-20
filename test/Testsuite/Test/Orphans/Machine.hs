{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Orphans.Machine () where



import qualified Data.Map              as M
import qualified Data.Text             as T
import           Test.Tasty.QuickCheck

import           Stg.Machine.Types

import           Test.Orphans.Language ()
import           Test.Orphans.Stack    ()
import           Test.Util


instance Arbitrary StgState where
    arbitrary = StgState
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary MemAddr where
    arbitrary = arbitrary1 MemAddr

instance Arbitrary ArgumentFrame where
    arbitrary = arbitrary1 ArgumentFrame

instance Arbitrary ReturnFrame where
    arbitrary = arbitrary2 ReturnFrame

instance Arbitrary UpdateFrame where
    arbitrary = arbitrary3 UpdateFrame

instance Arbitrary Value where
    arbitrary = oneof [ arbitrary1 Addr
                      , arbitrary1 PrimInt ]

instance Arbitrary Code where
    arbitrary = oneof [ arbitrary2 Eval
                      , arbitrary1 Enter
                      , arbitrary2 ReturnCon
                      , arbitrary1 ReturnInt ]

instance Arbitrary Globals where
    arbitrary = arbitrary1 (Globals . M.fromList)

instance Arbitrary Locals where
    arbitrary = arbitrary1 (Locals . M.fromList)

instance Arbitrary Closure where
    arbitrary = arbitrary2 Closure

instance Arbitrary Heap where
    arbitrary = arbitrary1 (Heap . M.fromList)

instance Arbitrary Info where
    arbitrary = oneof [ pure HaltedByPredicate
                      , pure MaxStepsExceeded
                      , arbitrary1 (NoRulesApply . fmap T.pack)
                      , arbitrary1 (StateError . T.pack)
                      , arbitrary1 (StateTransiton . T.pack) ]
