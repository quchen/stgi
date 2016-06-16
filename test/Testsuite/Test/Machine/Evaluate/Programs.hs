{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Tests of medium size, defined by terminating within a certain number of
-- steps (configured in 'defSpec').

-- These tests will be run with garbage collection enabled, and should have the
-- scope of small functions a Haskell beginner might play around with.
module Test.Machine.Evaluate.Programs (tests) where



import Data.Foldable
import Data.Monoid

import           Stg.Machine.Types
import           Stg.Marshal
import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude            as Stg

import qualified Test.Machine.Evaluate.TestTemplates.HaskellReference as HRef
import           Test.Machine.Evaluate.TestTemplates.MachineState
import           Test.Machine.Evaluate.TestTemplates.Util
import           Test.Orphans                                         ()
import           Test.QuickCheck.Modifiers
import           Test.Tasty



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

add3 :: TestTree
add3 = machineStateTest defSpec
    { testName = "add3 x y z = x+y+z"
    , source = [stg|
        add3 = \x y z -> case x of
            Int# i -> case y of
                Int# j -> case +# i j of
                    ij -> case z of
                        Int# k -> case +# ij k of
                            ijk -> Int# ijk;
                        badInt -> Error_add3_1 badInt;
                badInt -> Error_add3_2 badInt;
            badInt -> Error_add3_3 badInt;

        one   = \ -> Int# 1#;
        two   = \ -> Int# 2#;
        three = \ -> Int# 3#;
        main = \ => case add3 one two three of
            Int# i -> case i of
                6# -> Success;
                wrongResult -> TestFail wrongResult;
            badInt -> Error badInt
        |] }

takeRepeat :: TestTree
takeRepeat = machineStateTest defSpec
    { testName = "take 2 (repeat ())"
    , source = toStg "two" (2 :: Integer)
            <> Stg.take
            <> Stg.repeat
            <> Stg.foldr
            <> Stg.seq
            <> [stg|

        consBang = \x xs -> case xs of v -> Cons x v;
        nil = \ -> Nil;
        forceSpine = \xs -> foldr consBang nil xs;

        twoUnits = \ =>
            letrec
                repeated = \(unit) => repeat unit;
                unit = \ -> Unit;
                take2 = \(repeated) => take two repeated
            in forceSpine take2;

        main = \ => case twoUnits of
            Cons x xs -> case xs of
                Cons y ys -> case ys of
                    Nil -> Success;
                    default -> TestFailure;
                default -> TestFailure;
            default -> TestFailure
        |] }

fibonacci :: TestTree
fibonacci = machineStateTest defSpec
    { testName = "Fibonacci sequence"
    , source = Stg.equals_List_Int
            <> toStg "zero" (0 :: Int)
            <> toStg "one" (1 :: Int)
            <> toStg "numFibos" (numFibos :: Int)
            <> toStg "expectedFibos" (take numFibos fibo)
            <> Stg.add
            <> Stg.take
            <> Stg.zipWith
            <> [stg|

        main = \ =>
            letrec
                fibos = \(fibo) -> take numFibos fibo;
                fibo = \ =>
                    letrec
                        fib0 = \(fib1) -> Cons zero fib1;
                        fib1 = \(fib2) -> Cons one fib2;
                        fib2 = \(fib0 fib1) => zipWith add fib0 fib1
                    in fib0
            in case equals_List_Int fibos expectedFibos of
                True -> Success;
                err -> TestFail err
        |] }
  where
    fibo :: [Integer]
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
        , HRef.successPredicate = "main" `isLambdaForm` [stg| \ -> Success |]
        , HRef.failPredicate = const False
        , HRef.source = \(NonEmpty inputList) -> mconcat
                [ Stg.eq_Int
                , Stg.add
                , Stg.div
                , toStg "zero" (0 :: Int)
                , toStg "one"  (1 :: Int)
                , toStg "inputList" inputList
                , toStg "expectedOutput" (mean inputList) ]
            <> [stg|
            main = \ => case mean inputList of
                actual -> case eq_Int actual expectedOutput of
                    True -> Success;
                    False -> TestFailure actual;
                    badBool -> Error_badBool badBool
        |] }

meanNaive :: TestTree
meanNaive = HRef.haskellReferenceTest meanTestTemplate
    { HRef.testName = "Naïve: foldl and lazy tuple"
    , HRef.failWithInfo = True
    , HRef.source = \inputList -> HRef.source meanTestTemplate inputList
        <> Stg.foldl
        <> [stg|
        mean = \xs ->
            letrec
                totals = \(go zeroTuple) -> foldl go zeroTuple;
                zeroTuple = \ -> Tuple zero zero;
                go = \acc x -> case acc of
                    Tuple t n ->
                        let tx = \(t x) => add t x;
                            n1 = \(n) => add n one
                        in Tuple tx n1;
                    badTuple -> Error_mean1 badTuple
            in case totals xs of
                Tuple t n -> div t n;
                badTuple -> Error_mean2 badTuple
        |] }

meanNaiveWithFoldl' :: TestTree
meanNaiveWithFoldl' = HRef.haskellReferenceTest meanTestTemplate
    { HRef.testName = "Naïve with insufficient optimization: foldl'"
    , HRef.source = \inputList -> HRef.source meanTestTemplate inputList
        <> Stg.foldl'
        <> [stg|
        mean = \xs ->
            letrec
                totals = \(go zeroTuple) -> foldl' go zeroTuple;
                zeroTuple = \ -> Tuple zero zero;
                go = \acc x -> case acc of
                    Tuple t n ->
                        let tx = \(t x) => add t x;
                            n1 = \(n) => add n one
                        in Tuple tx n1;
                    badTuple -> Error_mean1 badTuple
            in case totals xs of
                Tuple t n -> div t n;
                badTuple -> Error_mean2 badTuple
        |] }

meanGood :: TestTree
meanGood = HRef.haskellReferenceTest meanTestTemplate
    { HRef.testName = "Proper: foldl' and strict tuple"
    , HRef.failWithInfo = False
    , HRef.failPredicate = \stgState -> length (stgStack stgState) >= 9
    , HRef.source = \inputList -> HRef.source meanTestTemplate inputList
        <> Stg.foldl'
        <> [stg|
        mean = \xs ->
            letrec
                totals = \(go zeroTuple) -> foldl' go zeroTuple;
                zeroTuple = \ -> Tuple zero zero;
                go = \acc x -> case acc of
                    Tuple t n ->
                        let tx = \(t x) => add t x;
                            n1 = \(n) => add n one
                        in case tx of
                            default -> case n1 of
                                default -> Tuple tx n1;
                    badTuple -> Error_mean1 badTuple
            in case totals xs of
                Tuple t n -> div t n;
                badTuple -> Error_mean2 badTuple
        |] }
