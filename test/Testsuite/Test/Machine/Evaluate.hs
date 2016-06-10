{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Machine.Evaluate (tests) where

-- TODO: Important tests to add:
--   - Only case does evaluation
--   - Don't forget to add the variables closed over in let(rec)



import qualified Test.Machine.Evaluate.Errors   as Errors
import qualified Test.Machine.Evaluate.Programs as Programs
import qualified Test.Machine.Evaluate.Rules    as Rules
import           Test.Tasty



tests :: TestTree
tests = testGroup "Evaluate"
    [ Rules.tests
    , Programs.tests
    , Errors.tests ]
