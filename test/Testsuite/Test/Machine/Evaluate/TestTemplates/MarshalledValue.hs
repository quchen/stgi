{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines tests of STG programs that are based on marshalling a value into
-- the STG, forcing a value, and marshalling that value out again for comparison
-- with a reference.
module Test.Machine.Evaluate.TestTemplates.MarshalledValue (
    MarshalledValueTestSpec(..),
    MarshalSourceSpec(..),
    defSpec,
    marshalledValueTest,
) where



import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import Stg.Language
import Stg.Language.Prettyprint
import Stg.Machine
import Stg.Machine.Types
import Stg.Marshal
import Stg.Parser.QuasiQuoter   (stg)

import Test.Machine.Evaluate.TestTemplates.Util
import Test.Orphans                             ()
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.Runners.Html



-- | Specifies a test that is based on marshalling a value out of the STG and
-- comparing it to a known value.
data MarshalledValueTestSpec input output = MarshalledValueTestSpec
    { testName :: Text
        -- ^ The reference function's name. Used only for display purposes.

    , failPredicate :: StgState -> Bool
        -- ^ Fail if this predicate holds. This can be used to constrain the
        -- heap size during the test, for example.

    , sourceSpec :: input -> MarshalSourceSpec output
        --  * STG program to run

    , maxSteps :: Integer
        -- ^ Maximum number of steps to take

    , failWithInfo :: Bool
        -- ^ Print program code and final state on test failure?
    }

data MarshalSourceSpec output = MarshalSourceSpec
    { resultVar     :: Var      -- ^ value to observe the value of, e.g. @main@
    , expectedValue :: output   -- ^ expected result value
    , source        :: Program  -- ^ STG program to run
    }

defSpec :: MarshalledValueTestSpec input b
defSpec = MarshalledValueTestSpec
    { testName = "Default Haskell reference test spec template"
    , maxSteps = 1024
    , failWithInfo = False
    , failPredicate = const False
    , sourceSpec = \_ -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = error "No expected value generator set in test"
        , source = [stg| main = \ -> DummySource |] }}

marshalledValueTest
    :: forall input output.
       ( Show input, Arbitrary input
       , Eq output, Show output, FromStg output, Pretty output )
    => MarshalledValueTestSpec input output
    -> TestTree
marshalledValueTest testSpec = askOption (\htmlOpt ->
    let pprDict = case htmlOpt of
            Just HtmlPath{} -> PrettyprinterDict prettyprintPlain (plain . pretty)
            Nothing         -> PrettyprinterDict prettyprint pretty
    in testProperty (T.unpack (testName testSpec)) (test pprDict) )
  where
    test :: ( Show input, Arbitrary input
            , Eq output, Show output, FromStg output, Pretty output )
         => PrettyprinterDict
         -> input
         -> Property
    test pprDict input =
        let program = initialState "main" (source (sourceSpec testSpec input))
            states = evalsUntil
                (RunForMaxSteps (maxSteps testSpec))
                (HaltIf (const False))
                (PerformGc (const Nothing))
                program
            verifyLoop (state :| _)
                | failPredicate testSpec state =
                    fail_failPredicateTrue pprDict testSpec input state
            verifyLoop (state :| rest) = case fromStg state (resultVar (sourceSpec testSpec input)) of
                Left err -> case err of
                    TypeMismatch -> fail_typeMismatch pprDict testSpec input state
                    IsBlackhole -> continue state rest
                    IsWrongLambdaType LambdaFun -> fail_functionValue pprDict testSpec input state
                    IsWrongLambdaType LambdaThunk -> continue state rest
                    IsWrongLambdaType LambdaCon -> error
                        "Critial error in test: found a constructor, expected\
                        \ a constructor, but still ran into the failure case\
                        \ somehow. Please report this as a bug."
                    BadArity -> fail_conArity pprDict testSpec input state
                    NotFound{} -> fail_notFound pprDict testSpec input state
                    AddrNotOnHeap -> fail_addrNotOnHeap pprDict testSpec input state
                    NoConstructorMatch -> fail_NoConstructorMatch pprDict testSpec input state
                Right actualValue -> assertEqual actualValue pprDict testSpec input state
            continue lastState = \case
                [] -> fail_valueNotFound pprDict testSpec input lastState
                (x:xs) -> verifyLoop (x :| xs)

        in verifyLoop states

assertEqual
    :: (Eq output, Pretty output)
    => output
    -> PrettyprinterDict
    -> MarshalledValueTestSpec input output
    -> input
    -> StgState
    -> Property
assertEqual
    actual
    (PrettyprinterDict pprText pprDoc)
    testSpec
    input
    finalState
  = counterexample failText (actual == expected)
  where
    expected = expectedValue (sourceSpec testSpec input)
    failText = (T.unpack . pprText . vsep)
        [ "Machine produced an invalid result."
        , "Expected:" <+> pprDoc expected
        , "Actual:  " <+> pprDoc actual
        , if failWithInfo testSpec
            then vsep
                [ hang 4 (vsep ["Program:", pprDoc (source (sourceSpec testSpec input))])
                , hang 4 (vsep ["Final state:", pprDoc finalState]) ]
            else failWithInfoInfoText ]

failWithInfoInfoText :: Doc
failWithInfoInfoText = "Run test case with failWithInfo to see the final state."

fail_template
    :: Doc
    -> PrettyprinterDict
    -> MarshalledValueTestSpec input a
    -> input
    -> StgState
    -> Property
fail_template
    failMessage
    (PrettyprinterDict pprText pprDoc)
    testSpec
    input
    finalState
  = counterexample failText False
  where
    failText = (T.unpack . pprText . vsep)
        [ failMessage
        , "Final machine state info:"
            <+> pprDoc (stgInfo finalState)
        , if failWithInfo testSpec
            then vsep
                [ hang 4 (vsep ["Program:", pprDoc (source (sourceSpec testSpec input))])
                , hang 4 (vsep ["Final state:", pprDoc finalState]) ]
            else failWithInfoInfoText ]

fail_failPredicateTrue, fail_valueNotFound, fail_typeMismatch, fail_conArity,
    fail_notFound, fail_addrNotOnHeap, fail_NoConstructorMatch, fail_functionValue
    :: PrettyprinterDict -> MarshalledValueTestSpec a b -> a -> StgState -> Property
fail_failPredicateTrue  = fail_template "Failure predicate held for an intemediate state"
fail_valueNotFound      = fail_template "None of the machine states produce a (marshallable)\
                                        \ value to compare the expected value to"
fail_typeMismatch       = fail_template "Type mismatch in input/expected output"
fail_conArity           = fail_template "Bad constructor arity in created value"
fail_notFound           = fail_template "Variable not found"
fail_addrNotOnHeap      = fail_template "Address not found on heap"
fail_NoConstructorMatch = fail_template "No constructor match"
fail_functionValue      = fail_template "Function value encountered; can only do algebraic"
