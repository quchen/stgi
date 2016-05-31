{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Prelude.Tuple (tests) where



import           Data.Monoid
import qualified Data.Tuple  as T

import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude            as Stg

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
    , source = \(tuple :: (Int, Int)) ->
           Stg.toStg "tuple" tuple
        <> Stg.toStg "expectedResult" (fst tuple)
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
    , source = \(tuple :: (Int, Int)) ->
           Stg.toStg "tuple" tuple
        <> Stg.toStg "expectedResult" (snd tuple)
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
    , source = \(x,y :: Int) ->
           Stg.toStg "x" x
        <> Stg.toStg "y" y
        <> Stg.toStg "expectedResult" (x+y)
        <> Stg.curry
        <> Stg.add
        <> Stg.eq_Int
        <> [stg|

        addPair = \tuple -> case tuple of
            Pair a b -> add a b;
            badPair  -> Error_addPair badTuple;

        main = \ =>
            let actual = \ -> curry addPair x y
            in case eq_Int expectedResult actual of
                True  -> Success;
                wrong -> TestFail wrong
        |] }

testUncurry :: TestTree
testUncurry = haskellReferenceTest defSpec
    { testName = "uncurry"
    , source = \(tuple :: (Int, Int)) ->
           Stg.toStg "tuple" tuple
        <> Stg.toStg "expectedResult" (uncurry (+) tuple)
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
    , source = \(tuple :: (Int, Int)) ->
           Stg.toStg "tuple" tuple
        <> Stg.toStg "expectedResult" (T.swap tuple)
        <> Stg.swap
        <> Stg.equals_Pair_Int
        <> [stg|

        main = \ =>
            let actual = \ -> swap tuple
            in case eq_Pair_Int expectedResult actual of
                True  -> Success;
                wrong -> TestFail wrong
        |] }
