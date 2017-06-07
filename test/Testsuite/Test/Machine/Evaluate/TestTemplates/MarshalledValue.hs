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



import           Data.List.NonEmpty                    (NonEmpty (..))
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import Stg.Language
import Stg.Language.Prettyprint
import Stg.Machine
import Stg.Machine.Types
import Stg.Marshal
import Stg.Parser.QuasiQuoter   (stg)

import Test.Orphans          ()
import Test.Tasty
import Test.Tasty.QuickCheck



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
       , Eq output, Show output, FromStg output, PrettyStgi output )
    => MarshalledValueTestSpec input output
    -> TestTree
marshalledValueTest testSpec = testProperty (T.unpack (testName testSpec)) test
  where
    test :: ( Show input, Arbitrary input
            , Eq output, Show output, FromStg output, PrettyStgi output )
         => input
         -> Property
    test input =
        let program = initialState "main" (source (sourceSpec testSpec input))
            states = evalsUntil
                (RunForMaxSteps (maxSteps testSpec))
                (HaltIf (const False))
                (PerformGc (const Nothing))
                program
            verifyLoop (state :| _)
                | failPredicate testSpec state =
                    fail_failPredicateTrue testSpec input state
            verifyLoop (state :| rest) = case fromStg state (resultVar (sourceSpec testSpec input)) of
                Left err -> case err of
                    TypeMismatch -> fail_typeMismatch testSpec input state
                    IsBlackhole -> continue state rest
                    IsWrongLambdaType LambdaFun -> fail_functionValue testSpec input state
                    IsWrongLambdaType LambdaThunk -> continue state rest
                    IsWrongLambdaType LambdaCon -> error
                        "Critial error in test: found a constructor, expected\
                        \ a constructor, but still ran into the failure case\
                        \ somehow. Please report this as a bug."
                    BadArity -> fail_conArity testSpec input state
                    NotFound{} -> fail_notFound testSpec input state
                    AddrNotOnHeap -> fail_addrNotOnHeap testSpec input state
                    NoConstructorMatch -> fail_NoConstructorMatch testSpec input state
                Right actualValue -> assertEqual actualValue testSpec input state
            continue lastState = \case
                [] -> fail_valueNotFound testSpec input lastState
                (x:xs) -> verifyLoop (x :| xs)

        in verifyLoop states

assertEqual
    :: forall input output. (Eq output, PrettyStgi output)
    => output
    -> MarshalledValueTestSpec input output
    -> input
    -> StgState
    -> Property
assertEqual
    actual
    testSpec
    input
    finalState
  = counterexample (failText expected) (actual == expected)
  where
    expected :: output
    expected = expectedValue (sourceSpec testSpec input)

    failText :: PrettyStgi a => a -> String
    failText ex = (T.unpack . renderStrict . layoutPretty defaultLayoutOptions . vsep)
        [ "Machine produced an invalid result."
        , "Expected:" <+> (prettyStgi ex :: Doc StgiAnn)
        , "Actual:  " <+> (prettyStgi actual :: Doc StgiAnn)
        , if failWithInfo testSpec
            then vsep
                [ hang 4 (vsep ["Program:", prettyStgi (source (sourceSpec testSpec input))])
                , hang 4 (vsep ["Final state:", prettyStgi finalState]) ]
            else failWithInfoInfoText ]

failWithInfoInfoText :: Doc ann
failWithInfoInfoText = "Run test case with failWithInfo to see the final state."

fail_template
    :: Doc StgiAnn
    -> MarshalledValueTestSpec input a
    -> input
    -> StgState
    -> Property
fail_template
    failMessage
    testSpec
    input
    finalState
  = counterexample failText False
  where
    failText = (T.unpack . renderStrict . layoutPretty defaultLayoutOptions . vsep)
        [ failMessage
        , "Final machine state info:"
            <+> (prettyStgi (stgInfo finalState) :: Doc StgiAnn)
        , if failWithInfo testSpec
            then vsep
                [ hang 4 (vsep ["Program:", prettyStgi (source (sourceSpec testSpec input)) :: Doc StgiAnn])
                , hang 4 (vsep ["Final state:", prettyStgi finalState :: Doc StgiAnn]) ]
            else failWithInfoInfoText ]

fail_failPredicateTrue, fail_valueNotFound, fail_typeMismatch, fail_conArity,
    fail_notFound, fail_addrNotOnHeap, fail_NoConstructorMatch, fail_functionValue
    :: MarshalledValueTestSpec a b -> a -> StgState -> Property
fail_failPredicateTrue  = fail_template "Failure predicate held for an intemediate state"
fail_valueNotFound      = fail_template "None of the machine states produced a (marshallable)\
                                        \ value to compare the expected value to"
fail_typeMismatch       = fail_template "Type mismatch in input/expected output"
fail_conArity           = fail_template "Bad constructor arity in created value"
fail_notFound           = fail_template "Variable not found"
fail_addrNotOnHeap      = fail_template "Address not found on heap"
fail_NoConstructorMatch = fail_template "No constructor match"
fail_functionValue      = fail_template "Function value encountered; can only do algebraic"
