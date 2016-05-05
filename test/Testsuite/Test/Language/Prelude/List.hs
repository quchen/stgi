{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Language.Prelude.List (tests) where



import qualified Data.List                                            as Reference
import           Data.Monoid

import qualified Stg.Language.Prelude                                 as Stg
import           Stg.Parser

import           Test.Machine.Evaluate.TestTemplates.HaskellReference
import           Test.Orphans                                         ()
import           Test.Tasty



tests :: TestTree
tests = testGroup "List"
    [ sort ]



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
