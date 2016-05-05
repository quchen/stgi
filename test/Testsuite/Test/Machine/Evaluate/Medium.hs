{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Tests of medium size, defined by terminating within a certain number of
-- steps (configured in 'defSpec').

-- These tests will be run with garbage collection enabled, and should have the
-- scope of small functions a Haskell beginner might play around with.
module Test.Machine.Evaluate.Medium (tests) where



import           Data.Monoid
import           Test.Tasty

import qualified Stg.Language.Prelude                             as Stg
import           Stg.Machine
import           Stg.Parser

import           Test.Machine.Evaluate.TestTemplates.MachineState
import           Test.Orphans                                     ()



tests :: TestTree
tests = testGroup "Medium-sized, with GC"
    [ program_add3
    , program_foldrSum
    , program_takeRepeat
    , program_map
    , program_zipWith
    , program_fibonacci ]

defSpec :: MachineStateTestSpec
defSpec = MachineStateTestSpec
    { testName         = "Default medium closure reduction test template"
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , source           = [stg| main = () \n () -> Success () |]
    , maxSteps         = 1024
    , performGc        = PerformGc (const True) }

program_add3 :: TestTree
program_add3 = machineStateTest defSpec
    { testName = "add3(x,y,z) = x+y+z"
    , source = [stgProgram|
        add3 = () \n (x,y,z) -> case x () of
            Int# (i) -> case y () of
                Int# (j) -> case +# i j of
                    ij -> case z () of
                        Int# (k) -> case +# ij k of
                            ijk -> Int# (ijk);
                        default -> Error ();
                default -> Error ();
            default -> Error ();

        one   = () \n () -> Int# (1#);
        two   = () \n () -> Int# (2#);
        three = () \n () -> Int# (3#);
        main = () \u () -> case add3 (one, two, three) of
            Int# (i) -> case i () of
                6# -> Success ();
                wrongResult -> TestFail (wrongResult);
            default -> Error ()
        |] }

program_foldrSum :: TestTree
program_foldrSum = machineStateTest defSpec
    { testName = "Sum of list via foldr"
    , source = Stg.foldr
            <> Stg.add
            <> Stg.int "zero" 0
            <> Stg.eq
            <> Stg.listOfNumbers "list" [1..5]
            <> Stg.int "expected" (sum [1..5])
            <> [stgProgram|
        sum = () \n (xs) -> foldr (add, zero, xs);
        main = () \u () ->
            let actual = () \u () -> sum (list)
            in case eq_Int (actual, expected) of
                True () -> Success ();
                default -> TestFail ()
        |] }

program_takeRepeat :: TestTree
program_takeRepeat = machineStateTest defSpec
    { testName = "take 2 (repeat ())"
    , source = Stg.int "two" 2
            <> Stg.take
            <> Stg.repeat
            <> Stg.foldr
            <> Stg.seq
            <> [stgProgram|

        consBang = () \n (x,xs) -> case xs () of v -> Cons (x, v);
        nil = () \n () -> Nil ();
        forceSpine = () \n (xs) -> foldr (consBang, nil, xs);

        twoUnits = () \u () ->
            letrec  repeated = (unit) \u () -> repeat (unit);
                    unit = () \n () -> Unit ();
                    take2 = (repeated) \u () -> take (two, repeated)
            in      forceSpine (take2);

        main = () \u () -> case twoUnits () of
            Cons (x,xs) -> case xs () of
                Cons (y,ys) -> case ys () of
                    Nil () -> Success ();
                    default -> TestFailure ();
                default -> TestFailure ();
            default -> TestFailure ()
        |] }

program_map :: TestTree
program_map = machineStateTest defSpec
    { testName = "map (+1) [1,2,3]"
    , source = Stg.add
            <> Stg.map
            <> Stg.listOfNumbers "inputList" [1,2,3]
            <> Stg.listOfNumbers "expectedResult" (map (+1) [1,2,3])
            <> Stg.equals_List_Int
            <> [stgProgram|

        main = () \u () ->
            letrec  plusOne = () \u () ->
                        letrec  one = () \n () -> Int# (1#);
                                plusOne' = (one) \n (n) -> add (n, one)
                        in plusOne' ();
                    actual = (plusOne) \u () -> map (plusOne, inputList)
            in case equals_List_Int (actual, expectedResult) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }

program_zipWith :: TestTree
program_zipWith = machineStateTest defSpec
    { testName = "zipWith (+)"
    , source = Stg.equals_List_Int
            <> Stg.listOfNumbers "list1" list1
            <> Stg.listOfNumbers "list2" list2
            <> Stg.listOfNumbers "expectedZipped" zipped
            <> Stg.add
            <> Stg.zipWith
            <> [stgProgram|

        main = () \u () ->
            let zipped = () \n () -> zipWith (add, list1, list2)
            in case equals_List_Int (zipped, expectedZipped) of
                True ()  -> Success ();
                False () -> TestFail ();
                err      -> Error_badBool (err)
        |] }
  where
    list1 = [1, 2, 3, 4, 5]
    list2 = [10, 20, 30]
    zipped = zipWith (+) list1 list2

program_fibonacci :: TestTree
program_fibonacci = machineStateTest defSpec
    { testName = "Fibonacci sequence"
    , source = Stg.equals_List_Int
            <> Stg.int "zero" 0
            <> Stg.int "one" 1
            <> Stg.int "numFibos" numFibos
            <> Stg.listOfNumbers "expectedFibos" (take numFibos fibo)
            <> Stg.add
            <> Stg.take
            <> Stg.zipWith
            <> [stgProgram|

        main = () \u () ->
            letrec
                fibos = (fibo) \n () -> take (numFibos, fibo);
                fibo = () \u () ->
                    letrec
                        fib0 = (fib1) \u () -> Cons (zero, fib1);
                        fib1 = (fib2) \u () -> Cons (one, fib2);
                        fib2 = (fib0, fib1) \u () -> zipWith (add, fib0, fib1)
                    in fib0 ()
            in case equals_List_Int (fibos, expectedFibos) of
                True () -> Success ();
                err -> TestFail (err)
        |] }
  where
    fibo = 0 : 1 : zipWith (+) fibo (tail fibo)
    numFibos :: Num a => a
    numFibos = 10
