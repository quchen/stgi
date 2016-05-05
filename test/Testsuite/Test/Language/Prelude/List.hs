{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Language.Prelude.List (tests) where


import           Prelude                                              hiding
                                                                       (filter)
import qualified Prelude                                              as P

import qualified Data.List                                            as Reference
import           Data.Monoid

import qualified Stg.Language.Prelude                                 as Stg
import           Stg.Parser

import           Test.Machine.Evaluate.TestTemplates.HaskellReference
import           Test.Orphans                                         ()
import           Test.Tasty



tests :: TestTree
tests = testGroup "List"
    [ sort, filter ]

filter :: TestTree
filter = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "filter"
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , source = \xs ->
           Stg.listOfNumbers "inputList" xs
        <> Stg.listOfNumbers "expectedResult" (P.filter (> 0) xs)
        <> Stg.int "zero" 0
        <> Stg.gt
        <> Stg.equals_List_Int
        <> Stg.filter
        <> [stgProgram|

        main = () \u () ->
            letrec
                positive = () \n (x) -> gt_Int (x, zero);
                filtered = (positive) \n () -> filter (positive, inputList)
            in case equals_List_Int (expectedResult, filtered) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |]
    , maxSteps = 1024 }

sort :: TestTree
sort = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "sort"
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , source = \xs ->
           Stg.listOfNumbers "inputList" xs
        <> Stg.listOfNumbers "expectedResult" (Reference.sort xs)
        <> Stg.equals_List_Int
        <> Stg.sort
        <> [stgProgram|

        main = () \u () ->
            let sorted = () \u () -> sort (inputList)
            in case equals_List_Int (expectedResult, sorted) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |]
    , maxSteps = 1024 }
