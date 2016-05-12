{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Tests of medium size, defined by terminating within a certain number of
-- steps (configured in 'defSpec').

-- These tests will be run with garbage collection enabled, and should have the
-- scope of small functions a Haskell beginner might play around with.
module Test.Machine.Evaluate.Programs (tests) where



import           Data.Foldable
import           Data.Monoid
import           Test.Tasty

import qualified Stg.Language.Prelude                                 as Stg
import           Stg.Machine
import           Stg.Machine.Types
import           Stg.Parser

import qualified Test.Machine.Evaluate.TestTemplates.HaskellReference as HRef
import           Test.Machine.Evaluate.TestTemplates.MachineState
import           Test.Orphans                                         ()
import           Test.QuickCheck.Modifiers



tests :: TestTree
tests = testGroup "Programs"
    [ add3
    , takeRepeat
    , fibonacci
    , testGroup "mean of a list"
        [ meanNaive
        , meanNaiveWithFoldl'
        , meanGood ]
    ]

defSpec :: MachineStateTestSpec
defSpec = MachineStateTestSpec
    { testName             = "Default program test template"
    , successPredicate     = "main" ===> [stg| () \n () -> Success () |]
    , forbiddenState       = const False
    , someStateSatisfies   = const True
    , source               = [stg| main = () \n () -> Success () |]
    , maxSteps             = 1024
    , performGc            = PerformGc (const True)
    , failWithInfo         = False }

add3 :: TestTree
add3 = machineStateTest defSpec
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

takeRepeat :: TestTree
takeRepeat = machineStateTest defSpec
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

fibonacci :: TestTree
fibonacci = machineStateTest defSpec
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

meanTestTemplate :: HRef.HaskellReferenceTestSpec (NonEmptyList Integer)
meanTestTemplate =
    let mean :: [Integer] -> Integer
        mean xs = let (total, count) = foldl' go (0,0) xs
                      go (!t, !c) x = (t+x, c+1)
                  in total `div` count
    in HRef.HaskellReferenceTestSpec
        { HRef.testName = "Mena test template"
        , HRef.maxSteps = 1024
        , HRef.failWithInfo = False
        , HRef.successPredicate = "main" ===> [stg| () \n () -> Success () |]
        , HRef.failPredicate = const False
        , HRef.source = \(NonEmpty inputList) -> mconcat
                [ Stg.eq
                , Stg.add
                , Stg.div
                , Stg.int "zero" 0
                , Stg.int "one" 1
                , Stg.listOfNumbers "inputList" inputList
                , Stg.int "expectedOutput" (mean inputList) ]
            <> [stgProgram|
            main = () \u () -> case mean (inputList) of
                actual -> case eq_Int (actual, expectedOutput) of
                    True () -> Success ();
                    False () -> TestFailure (actual);
                    badBool -> Error_badBool (badBool)
        |] }

meanNaive :: TestTree
meanNaive = HRef.haskellReferenceTest meanTestTemplate
    { HRef.testName = "Naïve: foldl and lazy tuple"
    , HRef.source = \inputList -> HRef.source meanTestTemplate inputList
        <> Stg.foldl
        <> [stgProgram|
        mean = () \n (xs) ->
            letrec
                totals = (go, zeroTuple) \n () -> foldl (go, zeroTuple);
                zeroTuple = () \n () -> Tuple (zero, zero);
                go = () \n (acc, x) -> case acc () of
                    Tuple (t,n) ->
                        let tx = (t,x) \u () -> add (t,x);
                            n1 = (n) \u () -> add (n,one)
                        in Tuple (tx, n1);
                    badTuple -> Error_mean1 (badTuple)
            in case totals (xs) of
                Tuple (t,n) -> div (t,n);
                badTuple -> Error_mean2 (badTuple)
        |] }

meanNaiveWithFoldl' :: TestTree
meanNaiveWithFoldl' = HRef.haskellReferenceTest meanTestTemplate
    { HRef.testName = "Naïve with insufficient optimization: foldl'"
    , HRef.source = \inputList -> HRef.source meanTestTemplate inputList
        <> Stg.foldl'
        <> [stgProgram|
        mean = () \n (xs) ->
            letrec
                totals = (go, zeroTuple) \n () -> foldl' (go, zeroTuple);
                zeroTuple = () \n () -> Tuple (zero, zero);
                go = () \n (acc, x) -> case acc () of
                    Tuple (t,n) ->
                        let tx = (t,x) \u () -> add (t,x);
                            n1 = (n) \u () -> add (n,one)
                        in Tuple (tx, n1);
                    badTuple -> Error_mean1 (badTuple)
            in case totals (xs) of
                Tuple (t,n) -> div (t,n);
                badTuple -> Error_mean2 (badTuple)
        |] }

meanGood :: TestTree
meanGood = HRef.haskellReferenceTest meanTestTemplate
    { HRef.testName = "Proper: foldl' and strict tuple"
    , HRef.failWithInfo = True
    , HRef.failPredicate = \stgState -> length (stgStack stgState) >= 9
    , HRef.source = \inputList -> HRef.source meanTestTemplate inputList
        <> Stg.foldl'
        <> [stgProgram|
        mean = () \n (xs) ->
            letrec
                totals = (go, zeroTuple) \n () -> foldl' (go, zeroTuple);
                zeroTuple = () \n () -> Tuple (zero, zero);
                go = () \n (acc, x) -> case acc () of
                    Tuple (t,n) ->
                        let tx = (t,x) \u () -> add (t,x);
                            n1 = (n) \u () -> add (n,one)
                        in case tx () of
                            default -> case n1 () of
                                default -> Tuple (tx, n1);
                    badTuple -> Error_mean1 (badTuple)
            in case totals (xs) of
                Tuple (t,n) -> div (t,n);
                badTuple -> Error_mean2 (badTuple)
        |] }
