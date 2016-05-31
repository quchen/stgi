{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Prelude.Bool (tests) where



import Data.Bool
import Data.Monoid

import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude            as Stg

import Test.Machine.Evaluate.TestTemplates.HaskellReference
import Test.Orphans                                         ()
import Test.Tasty



tests :: TestTree
tests = testGroup "Bool"
    [ testAnd2
    , testOr2
    , testNot
    , testBool ]

testAnd2 :: TestTree
testAnd2 = haskellReferenceTest defSpec
    { testName = "and2 (&&)"
    , source = \(b1, b2) ->
           Stg.toStg "b1" b1
        <> Stg.toStg "b2" b2
        <> Stg.toStg "expectedResult" (b1 && b2)
        <> Stg.eq_Bool
        <> Stg.and2
        <> [stg|
        main = \ => case and2 b1 b2 of
            result -> case eq_Bool result expectedResult of
                True    -> Success;
                False   -> TestFail result;
                badBool -> Error badBool
        |] }

testOr2 :: TestTree
testOr2 = haskellReferenceTest defSpec
    { testName = "or2 (||)"
    , source = \(b1, b2) ->
           Stg.toStg "b1" b1
        <> Stg.toStg "b2" b2
        <> Stg.toStg "expectedResult" (b1 || b2)
        <> Stg.eq_Bool
        <> Stg.or2
        <> [stg|
        main = \ => case or2 b1 b2 of
            result -> case eq_Bool result expectedResult of
                True -> Success;
                False -> TestFail result;
                badBool -> Error badBool
        |] }

testNot :: TestTree
testNot = haskellReferenceTest defSpec
    { testName = "not"
    , source = \b ->
           Stg.toStg "b" b
        <> Stg.toStg "expectedResult" (not b)
        <> Stg.eq_Bool
        <> Stg.not
        <> [stg|
        main = \ => case not b of
            result -> case eq_Bool result expectedResult of
                True -> Success;
                False -> TestFail result;
                badBool -> Error badBool
        |] }

testBool :: TestTree
testBool = haskellReferenceTest defSpec
    { testName = "bool"
    , maxSteps = 1024
    , failWithInfo = True
    , successPredicate = "main" ===> [stg| \ -> Success |]
    , failPredicate = const False
    , source = \(t :: Integer, f, p) ->
           Stg.toStg "p" p
        <> Stg.toStg "t" t
        <> Stg.toStg "f" f
        <> Stg.toStg "expectedResult" (bool t f p)
        <> Stg.eq_Int
        <> Stg.bool
        <> [stg|
        main = \ => case bool t f p of
            result -> case eq_Int result expectedResult of
                True -> Success;
                False -> TestFail result;
                badBool -> Error badBool
        |] }
