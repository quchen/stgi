{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Test.Language.Prelude.List (tests) where



import qualified Data.List   as L
import           Data.Monoid
import           Data.Text   (Text)

import           Stg.Language
import qualified Stg.Language.Prelude as Stg
import           Stg.Machine.Types
import           Stg.Parser

import Test.Machine.Evaluate.TestTemplates.HaskellReference
import Test.Orphans                                         ()
import Test.QuickCheck.Modifiers
import Test.Tasty



tests :: TestTree
tests = testGroup "List"
    [ stgConcat2
    , stgReverse
    , stgLength
    , stgCycle
    , stgIterate
    , stgRepeat
    , stgReplicate
    , stgSort
    , stgFilter
    , stgMap
    , stgZipWith
    , testGroup "Folds"
        [ stgFoldr
        , stgFoldl
        , stgFoldl' ]
    ]

stgFilter :: TestTree
stgFilter = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "filter"
    , maxSteps = 1024
    , failWithInfo = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \(xs, threshold) ->
           Stg.listOfNumbers "inputList" xs
        <> Stg.listOfNumbers "expectedResult" (filter (> threshold) xs)
        <> Stg.int "threshold" threshold
        <> Stg.gt
        <> Stg.equals_List_Int
        <> Stg.filter
        <> [stgProgram|

        main = () \u () ->
            letrec
                positive = () \n (x) -> gt_Int (x, threshold);
                filtered = (positive) \n () -> filter (positive, inputList)
            in case equals_List_Int (expectedResult, filtered) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }

stgSort :: TestTree
stgSort = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "sort"
    , maxSteps = 1024
    , failWithInfo = False
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
    , failWithInfo = False
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
    , failWithInfo = False
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
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }


stgFoldr, stgFoldl, stgFoldl' :: TestTree
stgFoldr  = foldSumTemplate
    "foldr"
    foldr
    (Stg.foldr <> [stg| fold = () \n () -> foldr  () |])
    (const False)
stgFoldl  = foldSumTemplate
    "foldl"
    foldl
    (Stg.foldl <> [stg| fold = () \n () -> foldl  () |])
    (const False)
stgFoldl' = foldSumTemplate
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
    , failWithInfo = False
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

stgConcat2 :: TestTree
stgConcat2 = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "(++)"
    , maxSteps = 1024
    , failWithInfo = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \(list1, list2) ->
           Stg.equals_List_Int
        <> Stg.listOfNumbers "list1" list1
        <> Stg.listOfNumbers "list2" list2
        <> Stg.listOfNumbers "expectedResult" (list1 ++ list2)
        <> Stg.concat2
        <> [stgProgram|

        main = () \u () ->
            let concatenated = () \n () -> concat2 (list1, list2)
            in case equals_List_Int (concatenated, expectedResult) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }

stgReverse :: TestTree
stgReverse = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "reverse"
    , maxSteps = 1024
    , failWithInfo = True
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \xs ->
           Stg.equals_List_Int
        <> Stg.listOfNumbers "input" xs
        <> Stg.listOfNumbers "expectedResult" (reverse xs)
        <> Stg.reverse
        <> [stgProgram|

        main = () \u () ->
            let reversed = () \n () -> reverse (input)
            in case equals_List_Int (reversed, expectedResult) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }

stgCycle :: TestTree
stgCycle = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "cycle (+take)"
    , maxSteps = 1024
    , failWithInfo = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \(NonEmpty list, NonNegative n) ->
           Stg.equals_List_Int
        <> Stg.int "n" n
        <> Stg.listOfNumbers "list" list
        <> Stg.listOfNumbers "expectedResult" (take (fromInteger n)
                                                    (cycle list) )
        <> Stg.take
        <> Stg.cycle
        <> [stgProgram|

        main = () \u () ->
            letrec
                cycled = () \n () -> cycle (list);
                takeCycled = (cycled) \n () -> take (n, cycled)
            in case equals_List_Int (takeCycled, expectedResult) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }

stgRepeat :: TestTree
stgRepeat = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "repeat (+take)"
    , maxSteps = 1024
    , failWithInfo = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \(item, NonNegative n) ->
           Stg.equals_List_Int
        <> Stg.int "n" n
        <> Stg.int "item" item
        <> Stg.listOfNumbers "expectedResult" (replicate (fromInteger n) item)
        <> Stg.take
        <> Stg.repeat
        <> [stgProgram|

        main = () \u () ->
            letrec
                repeated = () \n () -> repeat (item);
                takeRepeated = (repeated) \n () -> take (n, repeated)
            in case equals_List_Int (takeRepeated, expectedResult) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }

stgReplicate :: TestTree
stgReplicate = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "replicate"
    , maxSteps = 1024
    , failWithInfo = True
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = \stgState -> case stgCode stgState of
        Eval AppP {} _ -> True
        _ -> False
    , source = \(item, n) ->
           Stg.equals_List_Int
        <> Stg.int "n" n
        <> Stg.int "item" item
        <> Stg.listOfNumbers "expectedResult" (replicate (fromInteger n) item)
        <> Stg.take
        <> Stg.replicate
        <> [stgProgram|

        main = () \u () ->
            let replicated = () \n () -> replicate (n, item)
            in case equals_List_Int (replicated, expectedResult) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }

stgIterate :: TestTree
stgIterate = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "iterate (+take)"
    , maxSteps = 1024
    , failWithInfo = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \(seed, offset, NonNegative n) ->
           Stg.equals_List_Int
        <> Stg.int "n" n
        <> Stg.int "offset" offset
        <> Stg.int "seed" seed
        <> Stg.listOfNumbers "expectedResult" (take (fromInteger n)
                                                    (iterate (+offset) seed) )
        <> Stg.add
        <> Stg.take
        <> Stg.iterate
        <> [stgProgram|

        main = () \u () ->
            letrec
                addOffset = () \n () -> add (offset);
                iterated = (addOffset) \n () -> iterate (addOffset, seed);
                takeIterated = (iterated) \n () -> take (n, iterated)
            in case equals_List_Int (takeIterated, expectedResult) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }


stgLength :: TestTree
stgLength = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "length"
    , maxSteps = 1024
    , failWithInfo = False
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , failPredicate = const False
    , source = \xs ->
           Stg.eq
        <> Stg.int "expectedResult" (fromIntegral (length xs))
        <> Stg.listOfNumbers "input" xs
        <> Stg.length
        <> [stgProgram|

        main = () \u () ->
            let len = () \n () -> length (input)
            in case eq_Int (len, expectedResult) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |] }
