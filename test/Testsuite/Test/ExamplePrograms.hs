{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.ExamplePrograms (tests) where



import qualified Data.List as L

import Stg.ExamplePrograms
import Stg.Machine.Types

import qualified Test.Machine.Evaluate.TestTemplates.MachineState    as MST
import           Test.Machine.Evaluate.TestTemplates.MarshalledValue
import           Test.Orphans                                        ()
import           Test.QuickCheck.Modifiers
import           Test.Tasty



tests :: TestTree
tests = testGroup "Example programs"
    [ testGroup "Simple examples"
        [testImplies
        , testAddTwoNumbers
        , testCalculateLength ]
    , testGroup "map"
        [ testMapNot
        , testMapNotForced ]
    , testGroup "Sum using folds"
        [ testSumFoldl'
        , testSumFoldl
        , testSumFoldl'ViaFoldr
        , testSumFoldr ]
    , testGroup "Fibonacci"
        [ testFibonacciNaive
        , testFibonacciImproved
        , testFibonacciZipWith ]
    , testGroup "Concatenation"
        [ testListConcatLeftAssociated
        , testListConcatRightAssociated ]
    , testGroup "Sorting"
        [ testNaiveSort
        , testLibrarySort ]
    ]



testImplies :: TestTree
testImplies = marshalledValueTest defSpec
    { testName = "Implication"
    , sourceSpec = \(x,y) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = x <= y
        , source = implies x y }}

testAddTwoNumbers :: TestTree
testAddTwoNumbers = marshalledValueTest defSpec
    { testName = "Add two numbers"
    , sourceSpec = \(x,y) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = x+y
        , source = addTwoNumbers x y }}

testCalculateLength :: TestTree
testCalculateLength = marshalledValueTest defSpec
    { testName = "Length of a list"
    , sourceSpec = \(xs :: [Integer]) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = fromIntegral (length xs) :: Integer
        , source = calculateLength xs }}

testMapNot :: TestTree
testMapNot = MST.machineStateTest MST.defSpec
    { MST.testName = "map not, not forced"
    , MST.successPredicate = \state -> case stgInfo state of
        Info short _long -> short == NoRulesApply
    , MST.source = mapNot [True, False, True]
    , MST.maxSteps = 20}

testMapNotForced :: TestTree
testMapNotForced = marshalledValueTest defSpec
    { testName = "map not, forced"
    , sourceSpec = \xs -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = map not xs
        , source = mapNotForced xs }}

testSumFoldl' :: TestTree
testSumFoldl' = marshalledValueTest defSpec
    { testName = "Sum via foldl'"
    , sourceSpec = \xs -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = sum xs :: Integer
        , source = sum_foldl' xs }}

testSumFoldl :: TestTree
testSumFoldl = marshalledValueTest defSpec
    { testName = "Sum via foldl"
    , sourceSpec = \xs -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = sum xs :: Integer
        , source = sum_foldl xs }}

testSumFoldl'ViaFoldr :: TestTree
testSumFoldl'ViaFoldr = marshalledValueTest defSpec
    { testName = "Sum via foldl' implemented with foldr"
    , sourceSpec = \xs -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = sum xs :: Integer
        , source = sum_foldl'ViaFoldr xs }}

testSumFoldr :: TestTree
testSumFoldr = marshalledValueTest defSpec
    { testName = "Sum via foldr"
    , sourceSpec = \xs -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = sum xs :: Integer
        , source = sum_foldr xs }}

fibo :: Integer -> Integer
fibo = fibo' 0 1
  where
    fibo' !a !_b n | n <= 0 = a
    fibo'  a   b n = fibo' b (a+b) (n-1)

testFibonacciNaive :: TestTree
testFibonacciNaive = marshalledValueTest defSpec
    { testName = "Naive Fibonacci"
    , sourceSpec = \(NonNegative n) ->
        let smallerN = mod n 16
        in MarshalSourceSpec
            { resultVar = "main"
            , expectedValue = fibo smallerN
            , source = fibonacciNaive smallerN }}

testFibonacciImproved :: TestTree
testFibonacciImproved = marshalledValueTest defSpec
    { testName = "Improved linear Fibonacci"
    , sourceSpec = \(NonNegative n) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = fibo n
        , source = fibonacciImproved n }}

testFibonacciZipWith :: TestTree
testFibonacciZipWith = MST.machineStateTest MST.defSpec
    { MST.testName = "Fibonacci via zipWith"
    , MST.successPredicate = \state -> case stgCode state of
        ReturnCon "Int#" [PrimInt n] -> n == fibo 10
        _otherwise  -> False
    , MST.source = fibonacciZipWith }

testListConcatLeftAssociated :: TestTree
testListConcatLeftAssociated = marshalledValueTest defSpec
    { testName = "List concatenation, left-associated"
    , sourceSpec = \xss -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = concat xss :: [Integer]
        , source = listConcatLeftAssociated xss }}

testListConcatRightAssociated :: TestTree
testListConcatRightAssociated = marshalledValueTest defSpec
    { testName = "List concatenation, right-associated"
    , sourceSpec = \xss -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = concat xss :: [Integer]
        , source = listConcatRightAssociated xss }}

testNaiveSort :: TestTree
testNaiveSort = marshalledValueTest defSpec
    { testName = "Sort using naive Quicksort-inspired algorithm"
    , sourceSpec = \xs -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = L.sort xs :: [Integer]
        , source = naiveSort xs }}

testLibrarySort :: TestTree
testLibrarySort = marshalledValueTest defSpec
    { testName = "Sort using the standard Haskell list sort"
    , sourceSpec = \xs -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = L.sort xs :: [Integer]
        , source = librarySort xs }}
