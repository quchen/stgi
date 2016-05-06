{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Defines tests of STG programs that are meant to match their Haskell
-- sibling's behaviours.
module Test.Machine.Evaluate.TestTemplates.HaskellReference (
    HaskellReferenceTestSpec(..),
    haskellReferenceTest,
    (===>)
) where



import           Data.Monoid
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Stg.Language
import           Stg.Language.Prettyprint
import           Stg.Machine
import           Stg.Machine.Types
import           Stg.Parser

import           Test.Machine.Evaluate.TestTemplates.Util
import           Test.Orphans                             ()



-- | Specifies a test that is based on the reduction of a closure.
data HaskellReferenceTestSpec a = HaskellReferenceTestSpec
    { testName             :: Text
        -- ^ The reference function's name. Used only for display purposes.

    , successPredicate     :: StgState -> Bool
        -- ^ Test predicate to determine whether the desired state has been
        -- reached.

    , source               :: a -> Program
        -- ^ STG program to run.

    , maxSteps             :: Integer
        -- ^ Maximum number of steps to take

    , showFinalStateOnFail :: Bool
        -- ^ Print the full final state on failure? If set to 'False', only
        -- the 'stgInfo' will be printed.
    }

haskellReferenceTest
    :: (Show a, Arbitrary a)
    => HaskellReferenceTestSpec a
    -> TestTree
haskellReferenceTest testSpec = testProperty (T.unpack (testName testSpec)) test
  where
    test input =
        let program = initialState "main" (source testSpec input)
            finalState = evalUntil
                (RunForMaxSteps (maxSteps testSpec))
                (HaltIf ("main" ===> [stg| () \n () -> Success () |]))
                (PerformGc (const False))
                program
            failureText = (T.unpack . T.unlines)
                ([ "STG version of "
                    <> testName testSpec
                    <> " does not match Haskell's reference implementation."
                , "Failure because: "
                    <> prettyprintAnsi (stgInfo finalState)
                ]
                <>
                (if showFinalStateOnFail testSpec
                    then ["Final state:",  prettyprintAnsi finalState]
                    else [] ))
        in counterexample failureText (case stgInfo finalState of
            Info HaltedByPredicate _ -> True
            _otherwise               -> False )
