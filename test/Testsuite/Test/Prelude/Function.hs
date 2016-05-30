{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Prelude.Function (tests) where



import Data.Function
import Data.Monoid

import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude            as Stg

import Test.Machine.Evaluate.TestTemplates.HaskellReference
import Test.Orphans                                         ()
import Test.QuickCheck.Modifiers
import Test.Tasty



tests :: TestTree
tests = testGroup "Function"
    [ testId
    , testConst
    , testCompose
    , testFix ]

testId :: TestTree
testId = haskellReferenceTest defSpec
    { testName = "id"
    , source = \(x :: Integer) ->
           Stg.toStg "x" x
        <> Stg.eq_Int
        <> Stg.id
        <> [stg|
        main = \ =>
            case id x of
                idX -> case eq_Int idX x of
                    True -> Success;
                    False -> TestFail idX;
                    badBool -> Error badBool
        |] }

testConst :: TestTree
testConst = haskellReferenceTest defSpec
    { testName = "const"
    , source = \(x :: Integer, y :: Integer) ->
           Stg.toStg "x" x
        <> Stg.toStg "y" y
        <> Stg.eq_Int
        <> Stg.const
        <> [stg|
        main = \ =>
            case const x y of
                constXY -> case eq_Int constXY x of
                    True -> Success;
                    False -> TestFail constXY;
                    badBool -> Error badBool
        |] }


testCompose :: TestTree
testCompose = haskellReferenceTest defSpec
    { testName = "compose (.)"
    , source = \x ->
           Stg.toStg "x"     (x :: Integer)
        <> Stg.toStg "two"   (2 :: Integer)
        <> Stg.toStg "three" (3 :: Integer)
        <> Stg.toStg "expectedResult" (((*3) . (+2)) x)
        <> Stg.eq_Int
        <> Stg.add
        <> Stg.mul
        <> Stg.compose
        <> Stg.const
        <> [stg|

        plus2 = \x -> add x two;
        times3 = \x -> mul x three;
        plus2times3 = \ -> compose times3 plus2;
        main = \ =>
            case plus2times3 x of
                result -> case eq_Int result expectedResult of
                    True -> Success;
                    False -> TestFail result;
                    badBool -> Error badBool
        |] }

testFix :: TestTree
testFix = haskellReferenceTest defSpec
    { testName = "fix"
    , source = \(NonNegative (n :: Integer)) ->
           Stg.toStg "n" n
        <> Stg.toStg "zero" (0 :: Integer)
        <> Stg.toStg "one" (1 :: Integer)
        <> Stg.toStg "expectedResult"
                   (let fac' = \rec m -> if m == 0 then 1 else m * rec (m-1)
                        fac = fix fac'
                    in fac n )
        <> Stg.eq_Int
        <> Stg.sub
        <> Stg.mul
        <> Stg.fix
        <> Stg.const
        <> [stg|

        fac' = \rec m -> case eq_Int m zero of
            True -> one;
            False -> case sub m one of
                mMinusOne -> case rec mMinusOne of
                    recMMinusOne -> mul m recMMinusOne;
            badBool -> Error_fac' badBool;

        fac = \ => fix fac';

        main = \ =>
            case fac n of
                result -> case eq_Int result expectedResult of
                    True -> Success;
                    False -> TestFail result;
                    badBool -> Error badBool
        |] }
