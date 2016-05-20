{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Test.Language.Prelude.List (tests) where



import qualified Data.List   as L
import           Data.Monoid
import           Data.Text   (Text)

import           Stg.Language
import qualified Stg.Language.Prelude   as Stg
import           Stg.Machine.Types
import           Stg.Parser.QuasiQuoter

import Test.Machine.Evaluate.TestTemplates.HaskellReference
import Test.Orphans                                         ()
import Test.QuickCheck.Modifiers
import Test.Tasty



tests :: TestTree
tests = testGroup "List"
    [ testConcat2
    , testReverse
    , testLength
    , testCycle
    , testIterate
    , testRepeat
    , testReplicate
    , testSort
    , testFilter
    , testMap
    , testZip
    , testZipWith
    , testGroup "Folds"
        [ testFoldr
        , testFoldl
        , testFoldl' ]
    ]

testFilter :: TestTree
testFilter = haskellReferenceTest defSpec
    { testName = "filter"
    , source = \(xs, threshold) ->
           Stg.listOfNumbers "inputList" xs
        <> Stg.listOfNumbers "expectedResult" (filter (> threshold) xs)
        <> Stg.int "threshold" threshold
        <> Stg.gt_Int
        <> Stg.equals_List_Int
        <> Stg.filter
        <> [stg|

        main = \ =>
            letrec
                positive = \x -> gt_Int x threshold;
                filtered = \(positive) -> filter positive inputList
            in case equals_List_Int expectedResult filtered of
                True  -> Success;
                wrong -> TestFail wrong
        |] }

testSort :: TestTree
testSort = haskellReferenceTest defSpec
    { testName = "sort"
    , source = \xs ->
           Stg.listOfNumbers "inputList" xs
        <> Stg.listOfNumbers "expectedResult" (L.sort xs)
        <> Stg.equals_List_Int
        <> Stg.sort
        <> [stg|

        main = \ =>
            let sorted = \ => sort inputList
            in case equals_List_Int expectedResult sorted of
                True  -> Success;
                wrong -> TestFail wrong
        |] }

testMap :: TestTree
testMap = haskellReferenceTest defSpec
    { testName = "map"
    , source = \(xs, offset) ->
           Stg.add
        <> Stg.map
        <> Stg.int "offset" offset
        <> Stg.listOfNumbers "inputList" xs
        <> Stg.listOfNumbers "expectedResult" (map (+offset) xs)
        <> Stg.equals_List_Int
        <> [stg|

        main = \ =>
            letrec
                plusOffset = \n -> add n offset;
                actual = \(plusOffset) => map plusOffset inputList
            in case equals_List_Int actual expectedResult of
                True  -> Success;
                wrong -> TestFail wrong
        |] }

testZip :: TestTree
testZip = haskellReferenceTest defSpec
    { testName = "zip, map"
    , source = \(list1, list2) ->
           Stg.equals_List_Int
        <> Stg.listOfNumbers "list1" list1
        <> Stg.listOfNumbers "list2" list2
        <> Stg.listOfNumbers "expectedResult" (zipWith (+) list1 list2)
        <> Stg.add
        <> Stg.map
        <> Stg.uncurry
        <> Stg.zip
        <> [stg|

        main = \ =>
            letrec
                zipped   = \ -> zip list1 list2;
                addTuple = \ -> uncurry add;
                summed   = \(addTuple zipped) -> map addTuple zipped
            in case equals_List_Int summed expectedResult of
                True  -> Success;
                wrong -> TestFail wrong
        |] }

testZipWith :: TestTree
testZipWith = haskellReferenceTest defSpec
    { testName = "zipWith (+)"
    , source = \(list1, list2) ->
           Stg.equals_List_Int
        <> Stg.listOfNumbers "list1" list1
        <> Stg.listOfNumbers "list2" list2
        <> Stg.listOfNumbers "expectedResult" (zipWith (+) list1 list2)
        <> Stg.add
        <> Stg.zipWith
        <> [stg|

        main = \ =>
            let zipped = \ -> zipWith add list1 list2
            in case equals_List_Int zipped expectedResult of
                True  -> Success;
                wrong -> TestFail wrong
        |] }


testFoldr, testFoldl, testFoldl' :: TestTree
testFoldr  = foldSumTemplate
    "foldr"
    foldr
    (Stg.foldr <> [stg| fold = \ -> foldr |])
    (const False)
testFoldl  = foldSumTemplate
    "foldl"
    foldl
    (Stg.foldl <> [stg| fold = \ -> foldl |])
    (const False)
testFoldl' = foldSumTemplate
    "foldl'"
    L.foldl'
    (Stg.foldl' <> [stg| fold = \ -> foldl' |])
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
  = haskellReferenceTest defSpec
    { testName = foldName
    , maxSteps = 1024
    , failWithInfo = False
    , successPredicate = "main" ===> [stg| \ -> Success |]
    , failPredicate = failP
    , source = \(z, xs) ->
           foldStg
        <> Stg.add
        <> Stg.eq_Int
        <> Stg.int "z" z
        <> Stg.listOfNumbers "input" xs
        <> Stg.int "expected" (foldF (+) z xs)
        <> [stg|
        main = \ =>
            let actual = \ => fold add z input
            in case eq_Int actual expected of
                True  -> Success;
                defau -> TestFail
        |] }

testConcat2 :: TestTree
testConcat2 = haskellReferenceTest defSpec
    { testName = "(++)"
    , source = \(list1, list2) ->
           Stg.equals_List_Int
        <> Stg.listOfNumbers "list1" list1
        <> Stg.listOfNumbers "list2" list2
        <> Stg.listOfNumbers "expectedResult" (list1 ++ list2)
        <> Stg.concat2
        <> [stg|

        main = \ =>
            let concatenated = \ -> concat2 list1 list2
            in case equals_List_Int concatenated expectedResult of
                True  -> Success;
                wrong -> TestFail wrong
        |] }

testReverse :: TestTree
testReverse = haskellReferenceTest defSpec
    { testName = "reverse"
    , maxSteps = 1024
    , failWithInfo = True
    , successPredicate = "main" ===> [stg| \ -> Success |]
    , failPredicate = const False
    , source = \xs ->
           Stg.equals_List_Int
        <> Stg.listOfNumbers "input" xs
        <> Stg.listOfNumbers "expectedResult" (reverse xs)
        <> Stg.reverse
        <> [stg|

        main = \ =>
            let reversed = \ -> reverse input
            in case equals_List_Int reversed expectedResult of
                True  -> Success;
                wrong -> TestFail wrong
        |] }

testCycle :: TestTree
testCycle = haskellReferenceTest defSpec
    { testName = "cycle (+take)"
    , source = \(NonEmpty list, NonNegative n) ->
           Stg.equals_List_Int
        <> Stg.int "n" n
        <> Stg.listOfNumbers "list" list
        <> Stg.listOfNumbers "expectedResult" (take (fromInteger n)
                                                    (cycle list) )
        <> Stg.take
        <> Stg.cycle
        <> [stg|

        main = \ =>
            letrec
                cycled = \ -> cycle list;
                takeCycled = \(cycled) -> take n cycled
            in case equals_List_Int takeCycled expectedResult of
                True  -> Success;
                wrong -> TestFail wrong
        |] }

testRepeat :: TestTree
testRepeat = haskellReferenceTest defSpec
    { testName = "repeat (+take)"
    , source = \(item, NonNegative n) ->
           Stg.equals_List_Int
        <> Stg.int "n" n
        <> Stg.int "item" item
        <> Stg.listOfNumbers "expectedResult" (replicate (fromInteger n) item)
        <> Stg.take
        <> Stg.repeat
        <> [stg|

        main = \ =>
            letrec
                repeated = \ -> repeat item;
                takeRepeated = \(repeated) -> take n repeated
            in case equals_List_Int takeRepeated expectedResult of
                True  -> Success;
                wrong -> TestFail wrong
        |] }

testReplicate :: TestTree
testReplicate = haskellReferenceTest defSpec
    { testName = "replicate"
    , maxSteps = 1024
    , failWithInfo = True
    , successPredicate = "main" ===> [stg| \ -> Success |]
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
        <> [stg|

        main = \ =>
            let replicated = \ -> replicate n item
            in case equals_List_Int replicated expectedResult of
                True  -> Success;
                wrong -> TestFail wrong
        |] }

testIterate :: TestTree
testIterate = haskellReferenceTest defSpec
    { testName = "iterate (+take)"
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
        <> [stg|

        main = \ =>
            letrec
                addOffset = \ -> add offset;
                iterated = \(addOffset) -> iterate addOffset seed;
                takeIterated = \(iterated) -> take n iterated
            in case equals_List_Int takeIterated expectedResult of
                True  -> Success;
                wrong -> TestFail wrong
        |] }


testLength :: TestTree
testLength = haskellReferenceTest defSpec
    { testName = "length"
    , source = \xs ->
           Stg.eq_Int
        <> Stg.int "expectedResult" (fromIntegral (length xs))
        <> Stg.listOfNumbers "input" xs
        <> Stg.length
        <> [stg|

        main = \ =>
            let len = \ -> length input
            in case eq_Int len expectedResult of
                True  -> Success;
                wrong -> TestFail wrong
        |] }
