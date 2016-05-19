{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Test.Language.Prelude.Tuple (tests) where



import           Data.Monoid
import qualified Data.Tuple  as T

import qualified Stg.Language.Prelude   as Stg
import           Stg.Parser.QuasiQuoter

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
testFst = haskellReferenceTest defSpec
    { testName = "fst"
    , failWithInfo = True
    , source = \tuple ->
           Stg.tupleOfNumbers "tuple" tuple
        <> Stg.int "expectedResult" (fst tuple)
        <> Stg.fst
        <> Stg.eq_Int
        <> [stg|
        main = \ =>
            let actualFst = \ -> fst tuple
            in case eq_Int expectedResult actualFst of
                True  -> Success;
                wrong -> TestFail wrong
        |] }

testSnd :: TestTree
testSnd = haskellReferenceTest defSpec
    { testName = "snd"
    , source = \tuple ->
           Stg.tupleOfNumbers "tuple" tuple
        <> Stg.int "expectedResult" (snd tuple)
        <> Stg.snd
        <> Stg.eq_Int
        <> [stg|

        main = \ =>
            let actualSnd = \ -> snd tuple
            in case eq_Int expectedResult actualSnd of
                True  -> Success;
                wrong -> TestFail wrong
        |] }

testCurry :: TestTree
testCurry = haskellReferenceTest defSpec
    { testName = "curry"
    , source = \(x,y) ->
           Stg.int "x" x
        <> Stg.int "y" y
        <> Stg.int "expectedResult" (x+y)
        <> Stg.curry
        <> Stg.add
        <> Stg.eq_Int
        <> [stg|

        addTuple = \tuple -> case tuple of
            Tuple a b -> add a b;
            badTuple  -> Error_addTuple badTuple;

        main = \ =>
            let actual = \ -> curry addTuple x y
            in case eq_Int expectedResult actual of
                True  -> Success;
                wrong -> TestFail wrong
        |] }

testUncurry :: TestTree
testUncurry = haskellReferenceTest defSpec
    { testName = "uncurry"
    , source = \tuple ->
           Stg.tupleOfNumbers "tuple" tuple
        <> Stg.int "expectedResult" (uncurry (+) tuple)
        <> Stg.uncurry
        <> Stg.add
        <> Stg.eq_Int
        <> [stg|

        main = \ =>
            let actual = \ -> uncurry add tuple
            in case eq_Int expectedResult actual of
                True  -> Success;
                wrong -> TestFail wrong
        |] }

testSwap :: TestTree
testSwap = haskellReferenceTest defSpec
    { testName = "swap"
    , source = \tuple ->
           Stg.tupleOfNumbers "tuple" tuple
        <> Stg.tupleOfNumbers "expectedResult" (T.swap tuple)
        <> Stg.swap
        <> Stg.equals_Tuple_Int
        <> [stg|

        main = \ =>
            let actual = \ -> swap tuple
            in case eq_Tuple_Int expectedResult actual of
                True  -> Success;
                wrong -> TestFail wrong
        |] }
