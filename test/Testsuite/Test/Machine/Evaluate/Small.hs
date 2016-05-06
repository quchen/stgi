{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Tests of small size, defined by terminating within a certain number of
-- steps (configured in 'defSpec').
--
-- These tests will be run without garbage collection.
module Test.Machine.Evaluate.Small (tests) where



import           Test.Tasty

import           Stg.Machine
import           Stg.Parser

import           Test.Machine.Evaluate.TestTemplates.MachineState
import           Test.Orphans                                     ()



tests :: TestTree
tests = testGroup "Small, no GC"
    [ testGroup "Function application"
        [ funcapp_simple ]
    , testGroup "Case"
        [ testGroup "Default-only"
            [ defaultOnlyCase_unboundAlgebraic
            , defaultOnlyCase_boundAlgebraic
            , defaultOnlyCase_unboundPrimitive
            , defaultOnlyCase_boundPrimitive ]
        , testGroup "Algebraic alternatives"
            [ algebraicCase_normalMatch
            , algebraicCase_defaultUnboundMatch
            , algebraicCase_defaultBoundMatch ]
        , testGroup "Primitive alternatives"
            [ primitiveCase_normalMatch
            , primitiveCase_defaultUnboundMatch
            , primitiveCase_defaultBoundMatch ]
        ]
    , testGroup "Let"
        [ testGroup "Non-recursive"
            [ letBinding
            , letMultiBinding
            , letNestedBinding ]
        , testGroup "Recursive"
            [ letrecBinding
            , letrecMultiBinding ]
        ]
    , testGroup "Primitive functions"
        [ testGroup "Integer arithmetic"
            [ addition
            , subtraction
            , multiplication
            , division
            , modulo ]
        , testGroup "Integer comparisons"
            [ less
            , lessOrEqual
            , equal
            , unequal
            , greaterOrEqual
            , greater ]
        ]
    ]

defSpec :: MachineStateTestSpec
defSpec = MachineStateTestSpec
    { testName             = "Default small closure reduction test template"
    , successPredicate     = "main" ===> [stg| () \n () -> Success () |]
    , source               = [stg| main = () \n () -> Success () |]
    , maxSteps             = 32
    , performGc            = PerformGc (const False)
    , showFinalStateOnFail = False }

funcapp_simple :: TestTree
funcapp_simple = machineStateTest defSpec
    { testName = "Simple function application"
    , source = [stg|
        main = () \u () -> case id (unit) of
            Unit () -> Success ();
            default -> TestFail ();
        id = () \n (x) -> x ();
        unit = () \n () -> Unit ()
        |] }

defaultOnlyCase_unboundAlgebraic :: TestTree
defaultOnlyCase_unboundAlgebraic = machineStateTest defSpec
    { testName = "Unbound, algebraic scrutinee"
    , source = [stg|
        main = () \u () -> case x () of
            default -> x ();
        x = () \n () -> Success ()
        |] }

defaultOnlyCase_boundAlgebraic :: TestTree
defaultOnlyCase_boundAlgebraic = machineStateTest defSpec
    { testName = "Bound, algebraic scrutinee"
    , source = [stg|
        main = () \u () -> case x () of
            x -> x ();
        x = () \n () -> Success ()
        |] }

defaultOnlyCase_unboundPrimitive :: TestTree
defaultOnlyCase_unboundPrimitive = machineStateTest defSpec
    { testName = "Unbound, primitive scrutinee"
    , source = [stgProgram|
        main = () \u () -> case 1# of
            default -> Success ()
        |] }

defaultOnlyCase_boundPrimitive :: TestTree
defaultOnlyCase_boundPrimitive = machineStateTest defSpec
    { testName = "Bound, primitive scrutinee"
    , source = [stg|
        main = () \u () -> case 1# of
            x -> Success ()
        |] }

algebraicCase_normalMatch :: TestTree
algebraicCase_normalMatch = machineStateTest defSpec
    { testName = "Successful"
    , source = [stg|
        main = () \u () -> case Nothing () of
            Nothing () -> Success ();
            default    -> TestFail ()
        |] }

algebraicCase_defaultUnboundMatch :: TestTree
algebraicCase_defaultUnboundMatch = machineStateTest defSpec
    { testName = "Unbound default"
    , source = [stg|
        main = () \u () -> case Nothing () of
            Just (x) -> TestFail (x);
            default  -> Success ()
        |] }

algebraicCase_defaultBoundMatch :: TestTree
algebraicCase_defaultBoundMatch = machineStateTest defSpec
    { testName = "Bound default"
    , source = [stg|
        main = () \u () -> case Nothing () of
            Just (x) -> TestFail ();
            v -> Success ()

        |] }

primitiveCase_normalMatch :: TestTree
primitiveCase_normalMatch = machineStateTest defSpec
    { testName = "Successful"
    , source = [stg|
        main = () \u () -> case 1# of
            1#      -> Success ();
            default -> TestFail ()
        |] }

primitiveCase_defaultUnboundMatch :: TestTree
primitiveCase_defaultUnboundMatch = machineStateTest defSpec
    { testName = "Unbound default"
    , source = [stg|
        main = () \u () -> case 1# of
            0#      -> TestFail ();
            123#    -> TestFail ();
            default -> Success ()
        |] }

primitiveCase_defaultBoundMatch :: TestTree
primitiveCase_defaultBoundMatch = machineStateTest defSpec
    { testName = "Bound default"
    , source = [stg|
        main = () \u () -> case 1# of
            0#   -> TestFail ();
            123# -> TestFail ();
            -1#  -> TestFail ();
            x    -> Success ()
        |] }

letBinding :: TestTree
letBinding = machineStateTest defSpec
    { testName = "Single binding"
    , source = [stg|
        main = () \u () -> let x = () \n () -> Success ()
                           in x ()
        |] }

letMultiBinding :: TestTree
letMultiBinding = machineStateTest defSpec
    { testName = "Multiple bindings"
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
letNestedBinding = machineStateTest defSpec
    { testName = "Nested bindings"
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

letrecBinding :: TestTree
letrecBinding = machineStateTest defSpec
    { testName = "Single binding"
    , source = [stg|
        main = () \u () -> letrec x = () \n () -> Success ()
                           in x ()
        |] }

letrecMultiBinding :: TestTree
letrecMultiBinding = machineStateTest defSpec
    { testName = "Cross-referencing bindings"
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
addition = machineStateTest defSpec
    { testName = "Addition       +#"
    , source = [stg|
        op = () \n (x,y) -> case +# x y of
            v  -> Int# (v);
        main = () \u () -> case op (1#, 2#) of
            Int# (x) -> case x () of
                3# -> Success ();
                v  -> TestFail (v);
            default -> Error ()
        |] }

subtraction :: TestTree
subtraction = machineStateTest defSpec
    { testName = "Subtraction    -#"
    , source = [stg|
        op = () \n (x,y) -> case -# x y of
            v  -> Int# (v);
        main = () \u () -> case op (3#, 2#) of
            Int# (x) -> case x () of
                1# -> Success ();
                v  -> TestFail (v);
            default -> Error ()
        |] }

multiplication :: TestTree
multiplication = machineStateTest defSpec
    { testName = "Multiplication *#"
    , source = [stg|
        op = () \n (x,y) -> case *# x y of
            v  -> Int# (v);
        main = () \u () -> case op (3#, 2#) of
            Int# (x) -> case x () of
                6# -> Success ();
                v  -> TestFail (v);
            default -> Error ()
        |] }

division :: TestTree
division = machineStateTest defSpec
    { testName = "Division       /#"
    , source = [stg|
        op = () \n (x,y) -> case /# x y of
            v  -> Int# (v);
        main = () \u () -> case op (6#, 2#) of
            Int# (x) -> case x () of
                3# -> Success ();
                v  -> TestFail (v);
            default -> Error ()
        |] }

modulo :: TestTree
modulo = machineStateTest defSpec
    { testName = "Modulo         %#"
    , source = [stg|
        op = () \n (x,y) -> case %# x y of
            v  -> Int# (v);
        main = () \u () -> case op (5#, 2#) of
            Int# (x) -> case x () of
                1# -> Success ();
                v  -> TestFail (v);
            default -> Error ()
        |] }

less :: TestTree
less = machineStateTest defSpec
    { testName = "Less than        <#"
    , source = [stg|
        op = () \n (x,y) -> case <# x y of
            v  -> Int# (v);
        main = () \u () -> case op (1#, 2#) of
            Int# (x) -> case x () of
                1# -> Success ();
                v  -> TestFail (v);
            default -> Error ()
        |] }

lessOrEqual :: TestTree
lessOrEqual = machineStateTest defSpec
    { testName = "Less or equal    <=#"
    , source = [stg|
        op = () \n (x,y) -> case <=# x y of
            v  -> Int# (v);
        main = () \u () -> case op (1#, 2#) of
            Int# (x) -> case x () of
                1# -> Success ();
                v  -> TestFail (v);
            default -> Error ()
        |] }

equal :: TestTree
equal = machineStateTest defSpec
    { testName = "Equality         ==#"
    , source = [stg|
        op = () \n (x,y) -> case ==# x y of
            v  -> Int# (v);
        main = () \u () -> case op (2#, 2#) of
            Int# (x) -> case x () of
                1# -> Success ();
                v  -> TestFail (v);
            default -> Error ()
        |] }

unequal :: TestTree
unequal = machineStateTest defSpec
    { testName = "Inequality       /=#"
    , source = [stg|
        op = () \n (x,y) -> case /=# x y of
            v  -> Int# (v);
        main = () \u () -> case op (1#, 2#) of
            Int# (x) -> case x () of
                1# -> Success ();
                v  -> TestFail (v);
            default -> Error ()
        |] }

greaterOrEqual :: TestTree
greaterOrEqual = machineStateTest defSpec
    { testName = "Greater or equal >=#"
    , source = [stg|
        op = () \n (x,y) -> case >=# x y of
            v  -> Int# (v);
        main = () \u () -> case op (2#, 1#) of
            Int# (x) -> case x () of
                1# -> Success ();
                v  -> TestFail (v);
            default -> Error ()
        |] }

greater :: TestTree
greater = machineStateTest defSpec
    { testName = "Greater than     >#"
    , source = [stg|
        op = () \n (x,y) -> case ># x y of
            v  -> Int# (v);
        main = () \u () -> case op (2#, 1#) of
            Int# (x) -> case x () of
                1# -> Success ();
                v  -> TestFail (v);
            default -> Error ()
        |] }
