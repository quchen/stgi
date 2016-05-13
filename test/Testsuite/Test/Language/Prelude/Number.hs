{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Language.Prelude.Number (tests) where



import Data.Monoid

import qualified Stg.Language.Prelude as Stg
import           Stg.Parser

import Test.Machine.Evaluate.TestTemplates.HaskellReference
import Test.Orphans                                         ()
import Test.Tasty



tests :: TestTree
tests = testGroup "Number"
    [ testMin
    , testMax ]

testMin :: TestTree
testMin = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "filter"
    , maxSteps = 1024
    , failWithInfo = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \(x, y) ->
           Stg.int "x" x
        <> Stg.int "y" y
        <> Stg.int "expectedResult" (min x y)
        <> Stg.min
        <> Stg.eq_Int
        <> [stg|

        main = () \u () ->
            case min (x,y) of
                result -> case eq_Int (result, expectedResult) of
                    True () -> Success ();
                    False () -> TestFail (result);
                    badBool -> Error_testMin (badBool)
        |] }

testMax :: TestTree
testMax = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "filter"
    , maxSteps = 1024
    , failWithInfo = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \(x, y) ->
           Stg.int "x" x
        <> Stg.int "y" y
        <> Stg.int "expectedResult" (max x y)
        <> Stg.max
        <> Stg.eq_Int
        <> [stg|

        main = () \u () ->
            case max (x,y) of
                result -> case eq_Int (result, expectedResult) of
                    True () -> Success ();
                    False () -> TestFail (result);
                    badBool -> Error_testMin (badBool)
        |] }
