{-# LANGUAGE OverloadedStrings #-}

-- | Functions and types required to implement tests that check whether
-- a certain closure is reduced to the expected form by the STG.
module Test.Machine.Evaluate.TestTemplates.MachineState (
    MachineStateTestSpec(..),
    machineStateTest,
    (===>),
) where



import           Data.Monoid
import           Data.Text                                (Text)
import qualified Data.Text                                as T

import           Stg.Language
import           Stg.Language.Prettyprint
import           Stg.Machine
import           Stg.Machine.Types

import           Test.Machine.Evaluate.TestTemplates.Util
import           Test.Tasty
import           Test.Tasty.HUnit



-- | Specify a test that is based on a certain predicate to hold in an
-- evaluation step.
data MachineStateTestSpec = MachineStateTestSpec
    { testName             :: Text
        -- ^ Test name to display in the test overview.

    , successPredicate     :: StgState -> Bool
        -- ^ Test predicate to determine whether the desired state has been
        -- reached.

    , source               :: Program
        -- ^ STG program to run.

    , maxSteps             :: Integer
        -- ^ Maximum number of steps to take

    , performGc            :: PerformGc

    , showFinalStateOnFail :: Bool
        -- ^ Print the full final state on failure? If set to 'False', only
        -- the 'stgInfo' will be printed.
    }

-- | Evaluate the @main@ closure of a STG program, and check whether the
-- machine state satisfies a predicate when it is evaluated.
machineStateTest :: MachineStateTestSpec -> TestTree
machineStateTest testSpec = testCase (T.unpack (testName testSpec)) test
  where
    program = initialState "main" (source testSpec)
    finalState = evalUntil (RunForMaxSteps (maxSteps testSpec))
                           (HaltIf (successPredicate testSpec))
                           (performGc testSpec)
                           program
    test = case stgInfo finalState of
        Info HaltedByPredicate _ -> pure ()
        _otherwise -> (assertFailure . T.unpack . T.unlines)
            ([ "STG failed to satisfy predicate: "
                <> prettyprintAnsi (stgInfo finalState)
            ]
            <>
            (if showFinalStateOnFail testSpec
                then ["Final state:",  prettyprintAnsi finalState]
                else ["Run test case with showFinalStateOnFail enabled for more details."] ))
