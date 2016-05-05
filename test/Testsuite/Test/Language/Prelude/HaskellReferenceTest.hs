{-# LANGUAGE OverloadedStrings #-}

-- | Defines tests of STG programs that are meant to match their Haskell
-- sibling's behaviours.
module Test.Language.Prelude.HaskellReferenceTest (
    HaskellReferenceTestSpec(..),
    haskellReferenceTest,
) where



import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Stg.Language
import           Stg.Language.Prettyprint
import           Stg.Machine
import           Stg.Machine.Types

import           Test.Orphans             ()



-- | Specifies a test that is based on the reduction of a closure.
data HaskellReferenceTestSpec a b = HaskellReferenceTestSpec
    { testName         :: Text
        -- ^ Test name to display in the test overview.

    , testFunctionName :: Text
        -- ^ The Haskell/STG functions that are compared

    , testInput        :: a
        -- ^ Input given to the test. Will be provided to the STG.

    , haskellReference :: a -> b

    , successPredicate :: StgState -> Bool
        -- ^ Test predicate to determine whether the desired state has been
        -- reached.

    , source           :: Program
        -- ^ STG program to run.

    , maxSteps         :: Integer
        -- ^ Maximum number of steps to take
    }

haskellReferenceTest
    :: (Arbitrary a, Eq a)
    => HaskellReferenceTestSpec a
    -> TestTree
haskellReferenceTest testSpec = testProperty (T.unpack (testName testSpec)) test
  where
    program = initialState "main" (source testSpec)
    finalState = evalUntil (RunForMaxSteps (maxSteps testSpec))
                           (HaltIf (successPredicate testSpec))
                           (PerformGc (const False))
                           program
    test = counterexample failureText (case stgInfo finalState of
        Info HaltedByPredicate _ -> True
        _otherwise -> False)

    failureText = (T.unpack . T.unlines)
            [ "STG version of "
                <> testFunctionName testSpec
                <> " does not match Haskell's reference implementation."
            , "Failure because: "
                <> prettyprintAnsi (stgInfo finalState)
            , "Final state:"
            , prettyprintAnsi finalState ]
