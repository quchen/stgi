{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Tests of medium size. Attempt to limit them to testing a single reduction
-- step as much as possible.
--
-- These tests will be run without garbage collection.
module Test.Machine.Evaluate.Small (tests) where



import           Test.Tasty

import           Stg.Machine
import           Stg.Parser

import           Test.Machine.Evaluate.ClosureReductionTest
import           Test.Orphans                               ()



tests :: TestTree
tests = testGroup "Small, no GC"
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
        , letrecBinding
        , letMultiBinding
        , letNestedBinding
        , letrecMultiBinding ]
    , testGroup "Primitive functions"
        [ addition ]
    ]

defSpec :: ClosureReductionSpec
defSpec = ClosureReductionSpec
    { testName         = "Default small closure reduction test template"
    , successPredicate = "main" ==> [stg| () \n () -> Success () |]
    , source           = [stg| main = () \n () -> Success () |]
    , maxSteps         = 32
    , performGc        = PerformGc (const False) }

defaultOnlyCase_unboundAlgebraic :: TestTree
defaultOnlyCase_unboundAlgebraic = closureReductionTest defSpec
    { testName = "Unbound, algebraic scrutinee"
    , source = [stg|
        main = () \u () -> case x () of
            default -> x ();
        x = () \n () -> Success ()
        |] }

defaultOnlyCase_boundAlgebraic :: TestTree
defaultOnlyCase_boundAlgebraic = closureReductionTest defSpec
    { testName = "Bound, algebraic scrutinee"
    , source = [stg|
        main = () \u () -> case x () of
            x -> x ();
        x = () \n () -> Success ()
        |] }

defaultOnlyCase_unboundPrimitive :: TestTree
defaultOnlyCase_unboundPrimitive = closureReductionTest defSpec
    { testName = "Unbound, primitive scrutinee"
    , source = [stg|
        main = () \u () -> case x () of
            default -> Success ();
        x = () \n () -> 1#
        |] }

defaultOnlyCase_boundPrimitive :: TestTree
defaultOnlyCase_boundPrimitive = closureReductionTest defSpec
    { testName = "Bound, primitive scrutinee"
    , source = [stg|
        main = () \u () -> case x () of
            x -> Success ();
        x = () \n () -> 1#
        |] }

algebraicCase_normalMatch :: TestTree
algebraicCase_normalMatch = closureReductionTest defSpec
    { testName = "Successful"
    , source = [stg|
        main = () \u () -> case Nothing () of
            Nothing () -> Success ();
            default    -> TestFail ()
        |] }

algebraicCase_defaultUnboundMatch :: TestTree
algebraicCase_defaultUnboundMatch = closureReductionTest defSpec
    { testName = "Unbound default"
    , source = [stg|
        main = () \u () -> case Nothing () of
            Just (x) -> TestFail (x);
            default  -> Success ()
        |] }

algebraicCase_defaultBoundMatch :: TestTree
algebraicCase_defaultBoundMatch = closureReductionTest defSpec
    { testName = "Bound default"
    , source = [stg|
        main = () \u () -> case Nothing () of
            Just (x) -> TestFail ();
            v -> Success ()

        |] }

primitiveCase_normalMatch :: TestTree
primitiveCase_normalMatch = closureReductionTest defSpec
    { testName = "Successful"
    , source = [stg|
        main = () \u () -> case 1# of
            1#      -> Success ();
            default -> TestFail ()
        |] }

primitiveCase_defaultUnboundMatch :: TestTree
primitiveCase_defaultUnboundMatch = closureReductionTest defSpec
    { testName = "Unbound default"
    , source = [stg|
        main = () \u () -> case 1# of
            0#      -> TestFail ();
            123#    -> TestFail ();
            default -> Success ()
        |] }

primitiveCase_defaultBoundMatch :: TestTree
primitiveCase_defaultBoundMatch = closureReductionTest defSpec
    { testName = "Bound default"
    , source = [stg|
        main = () \u () -> case 1# of
            0#   -> TestFail ();
            123# -> TestFail ();
            -1#  -> TestFail ();
            x    -> Success ()
        |] }

letBinding :: TestTree
letBinding = closureReductionTest defSpec
    { testName = "let with a single binding"
    , source = [stg|
        main = () \u () -> let x = () \n () -> Success ()
                           in x ()
        |] }

letrecBinding :: TestTree
letrecBinding = closureReductionTest defSpec
    { testName = "letrec with a single binding"
    , source = [stg|
        main = () \u () -> letrec x = () \n () -> Success ()
                           in x ()
        |] }

letMultiBinding :: TestTree
letMultiBinding = closureReductionTest defSpec
    { testName = "let with two bindings"
    , source = [stg|
        main = () \u () ->
            let id = () \n (x) -> x ();
                one = () \n () -> Int# (1#)
            in case id (one) of
                Int# (y) -> case y () of
                    1# -> Success ();
                    wrong -> TestFail (wrong);
                default -> Error ()
        |] }

letNestedBinding :: TestTree
letNestedBinding = closureReductionTest defSpec
    { testName = "let with nested bindings"
    , source = [stg|
        main = () \u () ->
            let id = () \n (x) -> x ();
                one = () \n () -> Int# (1#)
            in let idOne = (id, one) \n () -> case id (one) of
                          v -> v ()
               in case idOne () of
                   Int# (y) -> case y () of
                       1# -> Success ();
                       wrong -> TestFail (wrong);
                   default -> Error ()
        |] }

letrecMultiBinding :: TestTree
letrecMultiBinding = closureReductionTest defSpec
    { testName = "letrec with nested bindings"
    , source = [stg|
        main = () \u () -> letrec id = () \n (x) -> x ();
                                  idOne = (id, one) \n () -> case id (one) of
                                      v -> v ();
                                  one = () \n () -> Int# (1#)
                           in case idOne () of
                               Int# (y) -> case y () of
                                   1# -> Success ();
                                   default -> TestFail ();
                               default -> Error ()
        |] }

addition :: TestTree
addition = closureReductionTest defSpec
    { testName = "Adding primitive numbers"
    , source = [stg|
        add = () \n (x,y) -> case +# x y of
            v  -> Int# (v);
        main = () \u () -> case add (1#, 2#) of
            Int# (x) -> case x () of
                3# -> Success ();
                v  -> TestFail (v);
            default -> Error ()
        |] }

funcapp_simple :: TestTree
funcapp_simple = closureReductionTest defSpec
    { testName = "Simple function application"
    , source = [stg|
        main = () \u () -> case id (unit) of
            Unit () -> Success ();
            default -> TestFail ();
        id = () \n (x) -> x ();
        unit = () \n () -> Unit ()
        |] }
