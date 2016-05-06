{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Test.Language.Prelude.List (tests) where



import qualified Data.List                                            as L
import           Data.Monoid
import           Data.Text                                            (Text)

import           Stg.Language
import qualified Stg.Language.Prelude                                 as Stg
import           Stg.Machine.Types
import           Stg.Parser

import           Test.Machine.Evaluate.TestTemplates.HaskellReference
import           Test.Orphans                                         ()
import           Test.Tasty



tests :: TestTree
tests = testGroup "List"
    [ stgSort
    , stgFilter
    , stgMap
    , stgZipWith
    , testGroup "Folds"
        [ foldrSum
        , foldlSum
        , foldl'Sum ]
    ]

stgFilter :: TestTree
stgFilter = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "filter"
    , maxSteps = 1024
    , showFinalStateOnFail = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \xs ->
           Stg.listOfNumbers "inputList" xs
        <> Stg.listOfNumbers "expectedResult" (filter (> 0) xs)
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
        |] }

stgSort :: TestTree
stgSort = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "sort"
    , maxSteps = 1024
    , showFinalStateOnFail = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \xs ->
           Stg.listOfNumbers "inputList" xs
        <> Stg.listOfNumbers "expectedResult" (L.sort xs)
        <> Stg.equals_List_Int
        <> Stg.sort
        <> [stgProgram|

        main = () \u () ->
            let sorted = () \u () -> sort (inputList)
            in case equals_List_Int (expectedResult, sorted) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }

stgMap :: TestTree
stgMap = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "map"
    , maxSteps = 1024
    , showFinalStateOnFail = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \(xs, offset) ->
           Stg.add
        <> Stg.map
        <> Stg.int "offset" offset
        <> Stg.listOfNumbers "inputList" xs
        <> Stg.listOfNumbers "expectedResult" (map (+offset) xs)
        <> Stg.equals_List_Int
        <> [stgProgram|

        main = () \u () ->
            letrec
                plusOffset = () \n (n) -> add (n, offset);
                actual = (plusOffset) \u () -> map (plusOffset, inputList)
            in case equals_List_Int (actual, expectedResult) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }

stgZipWith :: TestTree
stgZipWith = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "zipWith (+)"
    , maxSteps = 1024
    , showFinalStateOnFail = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \(list1, list2) ->
           Stg.equals_List_Int
        <> Stg.listOfNumbers "list1" list1
        <> Stg.listOfNumbers "list2" list2
        <> Stg.listOfNumbers "expectedResult" (zipWith (+) list1 list2)
        <> Stg.add
        <> Stg.zipWith
        <> [stgProgram|

        main = () \u () ->
            let zipped = () \n () -> zipWith (add, list1, list2)
            in case equals_List_Int (zipped, expectedResult) of
                True ()  -> Success ();
                wrong   -> TestFail (wrong)
        |] }


foldrSum, foldlSum, foldl'Sum :: TestTree
foldrSum  = foldSumTemplate
    "foldr"
    foldr
    (Stg.foldr <> [stg| fold = () \n () -> foldr  () |])
    (const False)
foldlSum  = foldSumTemplate
    "foldl"
    foldl
    (Stg.foldl <> [stg| fold = () \n () -> foldl  () |])
    (const False)
foldl'Sum = foldSumTemplate
    "foldl'"
    L.foldl'
    (Stg.foldl' <> [stg| fold = () \n () -> foldl' () |])
    (\stgState -> length (stgStack stgState) >= 8) -- Stack should stay small!

foldSumTemplate
    :: Text
        -- ^ Fold function name

    -> (forall a. (a -> a -> a) -> a -> [a] -> a)
        -- ^ Haskell reference fold function

    -> Program
        -- ^ STG Program with binding associating "fold" with the desired fold
        -- function, e.g. foldr

    -> (StgState -> Bool)
        -- ^ Failure predicate; useful in foldl' to check bounded stack size

    -> TestTree
foldSumTemplate foldName foldF foldStg failP
  = haskellReferenceTest HaskellReferenceTestSpec
    { testName = foldName
    , maxSteps = 1024
    , showFinalStateOnFail = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = failP
    , source = \(z, xs) ->
           foldStg
        <> Stg.add
        <> Stg.eq
        <> Stg.int "z" z
        <> Stg.listOfNumbers "input" xs
        <> Stg.int "expected" (foldF (+) z xs)
        <> [stgProgram|
        main = () \u () ->
            let actual = () \u () -> fold (add, z, input)
            in case eq_Int (actual, expected) of
                True () -> Success ();
                default -> TestFail ()
        |] }
