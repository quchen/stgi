{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines tests of STG programs that are meant to match their Haskell
-- sibling's behaviours.
module Test.Machine.Evaluate.TestTemplates.HaskellReference (
    HaskellReferenceTestSpec(..),
    defSpec,
    haskellReferenceTest,
    (===>),
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
import Stg.Parser.QuasiQuoter   (stg)

import Test.Machine.Evaluate.TestTemplates.Util
import Test.Orphans                             ()
import Test.Tasty.Runners.Html



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

defSpec :: HaskellReferenceTestSpec a
defSpec = HaskellReferenceTestSpec
    { testName = "Default Haskell reference test spec template"
    , maxSteps = 1024
    , failWithInfo = False
    , successPredicate = "main" ===> [stg| \ -> Success |]
    , failPredicate = const False
    , source = \_ -> [stg| main = \ -> DummySource |] }

haskellReferenceTest
    :: forall a. (Show a, Arbitrary a)
    => HaskellReferenceTestSpec a
    -> TestTree
haskellReferenceTest testSpec = askOption (\htmlOpt ->
    let pprDict = case htmlOpt of
            Just HtmlPath{} -> PrettyprinterDict prettyprintPlain (plain . pretty)
            Nothing         -> PrettyprinterDict prettyprint pretty
    in testProperty (T.unpack (testName testSpec)) (test pprDict) )
  where
    test :: (Show a, Arbitrary a)
         => PrettyprinterDict
         -> a
         -> Property
    test pprDict input =
        let program = initialState "main" (source testSpec input)
            states = evalsUntil
                (RunForMaxSteps (maxSteps testSpec))
                (HaltIf (successPredicate testSpec))
                (PerformGc (const Nothing))
                program
            finalState = last states
        in case L.find (failPredicate testSpec) states of
            Just badState -> fail_failPredicateTrue pprDict testSpec input badState
            Nothing -> case stgInfo finalState of
                Info HaltedByPredicate _ -> property True
                _otherwise -> fail_successPredicateNotTrue pprDict testSpec input finalState

failWithInfoInfoText :: Doc
failWithInfoInfoText = "Run test case with failWithInfo to see the final state."

fail_successPredicateNotTrue
    :: PrettyprinterDict
    -> HaskellReferenceTestSpec a
    -> a
    -> StgState
    -> Property
fail_successPredicateNotTrue
    (PrettyprinterDict pprText pprDoc)
    testSpec
    input
    finalState
  = counterexample failText False
  where
    failText = (T.unpack . pprText . vsep)
        [ "STG version of"
            <+> (text . T.unpack . testName) testSpec
            <+> "does not match Haskell's reference implementation."
        , "Failure because:"
            <+> pprDoc (stgInfo finalState)
        , if failWithInfo testSpec
            then vsep
                [ hang 4 (vsep ["Program:", pprDoc (source testSpec input)])
                , hang 4 (vsep ["Final state:", pprDoc finalState]) ]
            else failWithInfoInfoText ]

fail_failPredicateTrue
    :: PrettyprinterDict
    -> HaskellReferenceTestSpec a
    -> a
    -> StgState
    -> Property
fail_failPredicateTrue
    (PrettyprinterDict pprText pprDoc)
    testSpec
    input
    badState
  = counterexample failText False
  where
    failText = (T.unpack . pprText . vsep)
        [ "Failure predicate held for an intemediate state"
        , if failWithInfo testSpec
            then vsep
                [ hang 4 (vsep ["Program:", pprDoc (source testSpec input)])
                , hang 4 (vsep ["Bad state:" , pprDoc badState]) ]
            else failWithInfoInfoText ]
