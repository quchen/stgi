{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Defines tests of STG programs that are meant to match their Haskell
-- sibling's behaviours.
module Test.Machine.Evaluate.TestTemplates.HaskellReference (
    HaskellReferenceTestSpec(..),
    haskellReferenceTest,
    (===>)
) where



import qualified Data.List                    as L
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import Stg.Language
import Stg.Language.Prettyprint
import Stg.Machine
import Stg.Machine.Types
import Stg.Parser

import Test.Machine.Evaluate.TestTemplates.Util
import Test.Orphans                             ()



-- | Specifies a test that is based on the reduction of a closure.
data HaskellReferenceTestSpec a = HaskellReferenceTestSpec
    { testName :: Text
        -- ^ The reference function's name. Used only for display purposes.

    , successPredicate :: StgState -> Bool
        -- ^ Test predicate to determine whether the desired state has been
        -- reached.

    , failPredicate :: StgState -> Bool
        -- ^ Fail if this predicate holds. This can be used to constrain the
        -- heap size during the test, for example.

    , source :: a -> Program
        -- ^ STG program to run.

    , maxSteps :: Integer
        -- ^ Maximum number of steps to take

    , failWithInfo :: Bool
        -- ^ Print program code and final state on test failure?
    }

haskellReferenceTest :: (Show a, Arbitrary a)
    => HaskellReferenceTestSpec a
    -> TestTree
haskellReferenceTest testSpec = testProperty (T.unpack (testName testSpec)) test
  where
    test input =
        let program = initialState "main" (source testSpec input)
            states = evalsUntil
                (RunForMaxSteps (maxSteps testSpec))
                (HaltIf ("main" ===> [stg| () \n () -> Success () |]))
                (PerformGc (const False))
                program
            finalState = last states
            successPredicateNotTrueText = (T.unpack . prettyprintAnsi . vsep)
                [ "STG version of"
                    <+> (text . T.unpack . testName) testSpec
                    <+> "does not match Haskell's reference implementation."
                , "Failure because:"
                    <+> prettyAnsi (stgInfo finalState)
                , if failWithInfo testSpec
                    then vsep
                        [ hang 4 (vsep ["Program:", prettyAnsi (source testSpec input)])
                        , hang 4 (vsep ["Final state:", prettyAnsi finalState]) ]
                    else failWithInfoInfoText ]
            failurePredicateTrueText bad = (T.unpack . prettyprintAnsi . vsep)
                [ "Failure predicate held for an intemediate state"
                , if failWithInfo testSpec
                    then vsep
                        [ hang 4 (vsep ["Program:", prettyAnsi (source testSpec input)])
                        , hang 4 (vsep ["Bad state:" , prettyAnsi bad]) ]
                    else failWithInfoInfoText ]
        in case L.find (failPredicate testSpec) states of
            Just bad -> counterexample (failurePredicateTrueText bad) False
            Nothing -> case stgInfo finalState of
                Info HaltedByPredicate _ -> property True
                _otherwise -> counterexample successPredicateNotTrueText False

failWithInfoInfoText :: Doc
failWithInfoInfoText = "Run test case with failWithInfo to see the final state."
