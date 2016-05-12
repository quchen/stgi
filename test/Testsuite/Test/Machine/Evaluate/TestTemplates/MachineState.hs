{-# LANGUAGE OverloadedStrings #-}

-- | Functions and types required to implement tests that check whether
-- a certain closure is reduced to the expected form by the STG.
module Test.Machine.Evaluate.TestTemplates.MachineState (
    MachineStateTestSpec(..),
    machineStateTest,
    (===>),
) where



import qualified Data.List                    as L
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import Stg.Language
import Stg.Language.Prettyprint
import Stg.Machine
import Stg.Machine.Types

import Test.Machine.Evaluate.TestTemplates.Util
import Test.Tasty
import Test.Tasty.HUnit



-- | Specify a test that is based on a certain predicate to hold in an
-- evaluation step.
data MachineStateTestSpec = MachineStateTestSpec
    { testName :: Text
        -- ^ Test name to display in the test overview.

    , successPredicate :: StgState -> Bool
        -- ^ Test predicate to determine whether the desired state has been
        -- reached.

    , forbiddenState :: StgState -> Bool
        -- ^ Fail if this predicate holds. This can be used to constrain the
        -- heap size during the test, for example.

    , someStateSatisfies :: StgState -> Bool
        -- ^ Fail if this predicate held for no intermediate state. Useful to
        -- check whether some rule applied, for example.

    , source :: Program
        -- ^ STG program to run.

    , maxSteps :: Integer
        -- ^ Maximum number of steps to take

    , performGc :: PerformGc

    , failWithInfo :: Bool
        -- ^ Print program code and final state on test failure?
    }

-- | Evaluate the @main@ closure of a STG program, and check whether the
-- machine state satisfies a predicate when it is evaluated.
machineStateTest :: MachineStateTestSpec -> TestTree
machineStateTest testSpec = testCase (T.unpack (testName testSpec)) test
  where
    program = initialState "main" (source testSpec)
    states = evalsUntil (RunForMaxSteps (maxSteps testSpec))
                        (HaltIf (successPredicate testSpec))
                        (performGc testSpec)
                        program
    finalState = last states
    test = case L.find (forbiddenState testSpec) states of
        Just bad -> (assertFailure . T.unpack . prettyprintAnsi . vsep)
            [ "Failure predicate held for an intemediate state"
            , if failWithInfo testSpec
                then vsep
                    [ hang 4 (vsep ["Program:", prettyAnsi (source testSpec)])
                    , hang 4 (vsep ["Bad state:", prettyAnsi bad]) ]
                else failWithInfoInfoText ]
        Nothing -> case L.any (someStateSatisfies testSpec) states of
            True -> case stgInfo finalState of
                Info HaltedByPredicate _ -> pure ()
                _otherwise -> (assertFailure . T.unpack . prettyprintAnsi . vsep)
                    [ "STG failed to satisfy predicate: "
                        <> prettyAnsi (stgInfo finalState)
                    , if failWithInfo testSpec
                        then vsep
                            [ hang 4 (vsep ["Program:", prettyAnsi (source testSpec)])
                            , hang 4 (vsep ["Final state:", prettyAnsi finalState]) ]
                        else failWithInfoInfoText ]
            False -> (assertFailure . T.unpack)
                "No intermediate state satisfied the required predicate."

failWithInfoInfoText :: Doc
failWithInfoInfoText = "Run test case with failWithInfo to see the final state."
