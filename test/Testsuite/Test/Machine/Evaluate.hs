{-# LANGUAGE OverloadedStrings #-}

module Test.Machine.Evaluate (tests) where



import qualified Test.Machine.Evaluate.Errors   as Errors
import qualified Test.Machine.Evaluate.Programs as Programs
import qualified Test.Machine.Evaluate.Rules    as Rules
import           Test.Tasty



tests :: TestTree
tests = testGroup "Evaluate"
    [ Rules.tests
    , Programs.tests
    , Errors.tests ]
