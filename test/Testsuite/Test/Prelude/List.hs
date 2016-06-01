{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Prelude.List (tests) where



import qualified Data.List   as L
import           Data.Monoid
import           Data.Text   (Text)

import           Stg.Language
import           Stg.Machine.Types
import           Stg.Marshal
import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude            as Stg

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
    , source = \(xs, threshold :: Int) ->
           toStg "inputList" xs
        <> toStg "expectedResult" (filter (> threshold) xs)
        <> toStg "threshold" threshold
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
    , source = \(xs :: [Int]) ->
           toStg "inputList" xs
        <> toStg "expectedResult" (L.sort xs)
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
    , source = \(xs, offset :: Int) ->
           Stg.add
        <> Stg.map
        <> toStg "offset" offset
        <> toStg "inputList" xs
        <> toStg "expectedResult" (map (+offset) xs)
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
    , source = \(list1, list2 :: [Int]) ->
           Stg.equals_List_Int
        <> toStg "list1" list1
        <> toStg "list2" list2
        <> toStg "expectedResult" (zipWith (+) list1 list2)
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
    , source = \(list1, list2 :: [Int]) ->
           Stg.equals_List_Int
        <> toStg "list1" list1
        <> toStg "list2" list2
        <> toStg "expectedResult" (zipWith (+) list1 list2)
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
    , failPredicate = failP
    , source = \(z :: Int, xs) ->
           foldStg
        <> Stg.add
        <> Stg.eq_Int
        <> toStg "z" z
        <> toStg "input" xs
        <> toStg "expected" (foldF (+) z xs)
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
    , source = \(list1, list2 :: [Int]) ->
           Stg.equals_List_Int
        <> toStg "list1" list1
        <> toStg "list2" list2
        <> toStg "expectedResult" (list1 ++ list2)
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
    , failPredicate = const False
    , source = \(xs :: [Int]) ->
           Stg.equals_List_Int
        <> toStg "input" xs
        <> toStg "expectedResult" (reverse xs)
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
    , source = \(NonEmpty (list :: [Int]), NonNegative n) ->
           Stg.equals_List_Int
        <> toStg "n" n
        <> toStg "list" list
        <> toStg "expectedResult" (take n (cycle list))
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
    , source = \(item :: Int, NonNegative n) ->
           Stg.equals_List_Int
        <> toStg "n" n
        <> toStg "item" item
        <> toStg "expectedResult" (replicate n item)
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
    , failPredicate = \stgState -> case stgCode stgState of
        Eval AppP {} _ -> True
        _ -> False
    , source = \(item :: Int, n) ->
           Stg.equals_List_Int
        <> toStg "n" n
        <> toStg "item" item
        <> toStg "expectedResult" (replicate n item)
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
    , source = \(seed, offset :: Int, NonNegative n) ->
           Stg.equals_List_Int
        <> toStg "n" n
        <> toStg "offset" offset
        <> toStg "seed" seed
        <> toStg "expectedResult" (take n (iterate (+offset) seed) )
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
    , source = \(xs :: [Int]) ->
           Stg.eq_Int
        <> toStg "expectedResult" (length xs)
        <> toStg "input" xs
        <> Stg.length
        <> [stg|

        main = \ =>
            let len = \ -> length input
            in case eq_Int len expectedResult of
                True  -> Success;
                wrong -> TestFail wrong
        |] }
