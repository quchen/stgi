{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Test.Language.Prelude.Tuple (tests) where



import           Data.Monoid
import qualified Data.Tuple  as T

import qualified Stg.Language.Prelude as Stg
import           Stg.Parser

import Test.Machine.Evaluate.TestTemplates.HaskellReference
import Test.Orphans                                         ()
import Test.Tasty



tests :: TestTree
tests = testGroup "Tuple"
    [ testFst
    , testSnd
    , testCurry
    , testUncurry
    , testSwap
    ]

testFst :: TestTree
testFst = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "fst"
    , maxSteps = 1024
    , failWithInfo = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \tuple ->
           Stg.tupleOfNumbers "tuple" tuple
        <> Stg.int "expectedResult" (fst tuple)
        <> Stg.fst
        <> Stg.eq_Int
        <> [stg|
        main = () \u () ->
            let actualFst = () \n () -> fst (tuple)
            in case eq_Int (expectedResult, actualFst) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }

testSnd :: TestTree
testSnd = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "snd"
    , maxSteps = 1024
    , failWithInfo = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \tuple ->
           Stg.tupleOfNumbers "tuple" tuple
        <> Stg.int "expectedResult" (snd tuple)
        <> Stg.snd
        <> Stg.eq_Int
        <> [stg|

        main = () \u () ->
            let actualSnd = () \n () -> snd (tuple)
            in case eq_Int (expectedResult, actualSnd) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }

testCurry :: TestTree
testCurry = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "curry"
    , maxSteps = 1024
    , failWithInfo = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \(x,y) ->
           Stg.int "x" x
        <> Stg.int "y" y
        <> Stg.int "expectedResult" (x+y)
        <> Stg.curry
        <> Stg.add
        <> Stg.eq_Int
        <> [stg|

        addTuple = () \n (tuple) -> case tuple () of
            Tuple (a,b) -> add (a,b);
            badTuple -> Error_addTuple (badTuple);

        main = () \u () ->
            let actual = () \n () -> curry (addTuple, x, y)
            in case eq_Int (expectedResult, actual) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }

testUncurry :: TestTree
testUncurry = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "uncurry"
    , maxSteps = 1024
    , failWithInfo = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \tuple ->
           Stg.tupleOfNumbers "tuple" tuple
        <> Stg.int "expectedResult" (uncurry (+) tuple)
        <> Stg.uncurry
        <> Stg.add
        <> Stg.eq_Int
        <> [stg|

        main = () \u () ->
            let actual = () \n () -> uncurry (add, tuple)
            in case eq_Int (expectedResult, actual) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }

testSwap :: TestTree
testSwap = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "swap"
    , maxSteps = 1024
    , failWithInfo = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \tuple ->
           Stg.tupleOfNumbers "tuple" tuple
        <> Stg.tupleOfNumbers "expectedResult" (T.swap tuple)
        <> Stg.swap
        <> Stg.equals_Tuple_Int
        <> [stg|

        main = () \u () ->
            let actual = () \n () -> swap (tuple)
            in case eq_Tuple_Int (expectedResult, actual) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }
