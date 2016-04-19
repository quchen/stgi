{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Machine.Evaluate (tests) where



import           Data.Monoid
import           Data.Text
import qualified Data.Text                as T
import           Test.Tasty
import           Test.Tasty.HUnit

import           Stg.Language
import           Stg.Language.Prettyprint
import           Stg.Machine
import           Stg.Machine.Env
import           Stg.Machine.Heap         as H
import           Stg.Machine.Types
import           Stg.Parser

import           Test.Orphans.Machine     ()



tests :: TestTree
tests = testGroup "Evaluate"
    [ testGroup "Closure reduction"
        [ testGroup "Function application"
            [ funcapp_simple ]
        , testGroup "Default-only case"
            [ defaultOnlyCase_unboundAlgebraic
            , defaultOnlyCase_boundAlgebraic
            , defaultOnlyCase_unboundPrimitive
            , defaultOnlyCase_boundPrimitive ]
        , testGroup "Algebraic case"
            [ algebraicCase_normalMatch
            , algebraicCase_defaultUnboundMatch
            , algebraicCase_defaultBoundMatch ]
        , testGroup "Primitive case"
            [ primitiveCase_normalMatch
            , primitiveCase_defaultUnboundMatch
            , primitiveCase_defaultBoundMatch
            ]
        , testGroup "Let"
            [ letBinding
            , letrecBinding ]
        , testGroup "Primitive functions"
            [ addition ]
        ]
    ]

defaultOnlyCase_unboundAlgebraic :: TestTree
defaultOnlyCase_unboundAlgebraic = closureReductionTest ClosureReductionSpec
    { testName = "Unbound, algebraic scrutinee"
    , successPredicate = "main" ==> [stg| () \n () -> Success () |]
    , source = [stg|
        main = () \u () -> case x () of
            default -> x ();
        x = () \n () -> Success ()
        |] }

defaultOnlyCase_boundAlgebraic :: TestTree
defaultOnlyCase_boundAlgebraic = closureReductionTest ClosureReductionSpec
    { testName = "Bound, algebraic scrutinee"
    , successPredicate = "main" ==> [stg| () \n () -> Success () |]
    , source = [stg|
        main = () \u () -> case x () of
            x -> x ();
        x = () \n () -> Success ()
        |] }

defaultOnlyCase_unboundPrimitive :: TestTree
defaultOnlyCase_unboundPrimitive = closureReductionTest ClosureReductionSpec
    { testName = "Unbound, primitive scrutinee"
    , successPredicate = "main" ==> [stg| () \n () -> 1# |]
    , source = [stg|
        main = () \u () -> case x () of
            default -> x ();
        x = () \n () -> 1#
        |] }

defaultOnlyCase_boundPrimitive :: TestTree
defaultOnlyCase_boundPrimitive = closureReductionTest ClosureReductionSpec
    { testName = "Bound, primitive scrutinee"
    , successPredicate = "main" ==> [stg| () \n () -> 1# |]
    , source = [stg|
        main = () \u () -> case x () of
            x -> x ();
        x = () \n () -> 1#
        |] }

algebraicCase_normalMatch :: TestTree
algebraicCase_normalMatch = closureReductionTest ClosureReductionSpec
    { testName = "Successful"
    , successPredicate = "main" ==> [stg| () \n () -> Success () |]
    , source = [stg|
        main = () \u () -> case Nothing () of
            Nothing () -> Success ();
            default    -> Fail ()
        |] }

algebraicCase_defaultUnboundMatch :: TestTree
algebraicCase_defaultUnboundMatch = closureReductionTest ClosureReductionSpec
    { testName = "Unbound default"
    , successPredicate = "main" ==> [stg| () \n () -> Success () |]
    , source = [stg|
        main = () \u () -> case Nothing () of
            Just (x) -> Fail ();
            default  -> Success ()
        |] }

algebraicCase_defaultBoundMatch :: TestTree
algebraicCase_defaultBoundMatch = closureReductionTest ClosureReductionSpec
    { testName = "Bound default"
    , successPredicate = "main" ==> [stg| () \n () -> Nothing () |]
    , source = [stg|
        main = () \u () -> case Nothing () of
            Just (x) -> Fail ();
            v -> v ()

        |] }

primitiveCase_normalMatch :: TestTree
primitiveCase_normalMatch = closureReductionTest ClosureReductionSpec
    { testName = "Successful"
    , successPredicate = "main" ==> [stg| () \n () -> Success () |]
    , source = [stg|
        main = () \u () -> case 1# of
            1#      -> Success ();
            default -> Fail ()
        |] }

primitiveCase_defaultUnboundMatch :: TestTree
primitiveCase_defaultUnboundMatch = closureReductionTest ClosureReductionSpec
    { testName = "Unbound default"
    , successPredicate = "main" ==> [stg| () \n () -> Success () |]
    , source = [stg|
        main = () \u () -> case 1# of
            0#      -> Fail ();
            123#    -> Fail ();
            default -> Success ()
        |] }

primitiveCase_defaultBoundMatch :: TestTree
primitiveCase_defaultBoundMatch = closureReductionTest ClosureReductionSpec
    { testName = "Bound default"
    , successPredicate = "main" ==> [stg| () \n () -> 1# |]
    , source = [stg|
        main = () \u () -> case 1# of
            0#   -> Fail ();
            123# -> Fail ();
            x    -> x ()
        |] }

letBinding :: TestTree
letBinding = closureReductionTest ClosureReductionSpec
    { testName = "Let binding"
    , successPredicate = "main" ==> [stg| () \n () -> Success () |]
    , source = [stg|
        main = () \u () -> let x = () \n () -> Success ()
                           in x ()
        |] }

letrecBinding :: TestTree
letrecBinding = closureReductionTest ClosureReductionSpec
    { testName = "Let binding"
    , successPredicate = "main" ==> [stg| () \n () -> Success () |]
    , source = [stg|
        main = () \u () -> letrec x = () \n () -> Success ()
                           in x ()
        |] }

addition :: TestTree
addition = closureReductionTest ClosureReductionSpec
    { testName = "Adding numbers"
    , successPredicate = "main" ==> [stg| () \n () -> 3# |]
    , source = [stg|
        add = () \n (x, y) -> case +# x y of
            v -> Int (v);
        main = () \u () -> add (1#, 2#)
        |] }

funcapp_simple :: TestTree
funcapp_simple = closureReductionTest ClosureReductionSpec
    { testName = "Simple function application"
    , successPredicate = "main" ==> [stg| () \n () -> Success () |]
    , source = [stg|
        main = () \u () -> case id (unit) of
            Unit () -> Success ();
            default -> Fail ();
        id = () \n (x) -> x ();
        unit = () \n () -> Unit ()
        |] }

-- | Specifies a test that is based on the reduction of a closure.
data ClosureReductionSpec = ClosureReductionSpec
    { testName         :: Text
        -- ^ Test name to display in the test overview.

    , successPredicate :: StgState -> Bool
        -- ^ Test predicate to determine whether the desired state has been
        -- reached.

    , source           :: Program
        -- ^ STG program to run.
    }

-- | Evaluate the @main@ closure of a STG program, and check whether the
-- machine state satisfies a predicate when it is evaluated.
closureReductionTest :: ClosureReductionSpec -> TestTree
closureReductionTest testSpec = testCase (T.unpack (testName testSpec)) test
  where
    program = initialState "main" (source testSpec)
    finalState = evalUntil 1e3 (successPredicate testSpec) program
    test = case stgInfo finalState of
        HaltedByPredicate -> pure ()
        _otherwise -> (assertFailure . T.unpack . T.unlines)
            [ "STG failed to satisfy predicate: "
                <> prettyprintAnsi (stgInfo finalState)
            , "Final state:"
            , prettyprintAnsi finalState ]

-- | Build a state predicate that asserts that a certain 'Var' maps to
-- a 'LambdaForm' in the heap.
(==>) :: Var -> LambdaForm -> StgState -> Bool
var ==> lambdaForm = \state -> case varLookup state var of
    VarLookupClosure (Closure lf _) -> lf == lambdaForm
    _otherwise                      -> False

-- | Used as the result of 'varLookup'.
data VarLookupResult =
      VarLookupError Text
    | VarLookupPrim Integer
    | VarLookupClosure Closure
    deriving (Eq, Ord, Show)

-- | Look up the value of a 'Var' on the 'Heap' of a 'StgState'.
varLookup :: StgState -> Var -> VarLookupResult
varLookup state var =
    case globalVal (stgGlobals state) var of
        Nothing -> VarLookupError "not found in globals"
        Just (Addr addr) -> case H.lookup addr (stgHeap state) of
            Just closure -> VarLookupClosure closure
            Nothing -> VarLookupError "not found on heap"
        Just (PrimInt i) -> VarLookupPrim i
