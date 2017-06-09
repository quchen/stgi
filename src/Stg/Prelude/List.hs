{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}

-- | Definitions found in Haskell's "Data.List".
--
-- This module should be imported qualified to avoid clashes with standard
-- Haskell definitions.
module Stg.Prelude.List (
    nil,
    concat2,
    reverse,
    foldl,
    foldl',
    foldr,
    iterate,
    cycle,
    take,
    filter,
    partition,
    repeat,
    replicate,
    sort,
    naiveSort,
    map,
    equals_List_Int,
    length,
    zip,
    zipWith,
    forceSpine,
) where



import Prelude ()

import Data.Monoid

import Stg.Language
import Stg.Parser.QuasiQuoter
import Stg.Prelude.Function   as Func
import Stg.Prelude.Number     as Num

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Stg.Language.Prettyprint
-- >>> import qualified Data.Text.IO   as T



nil, concat2, foldl, foldl', foldr, iterate, cycle, take, filter,
    repeat, replicate, sort, map, equals_List_Int, length, zip, zipWith,
    reverse, forceSpine, naiveSort, partition :: Program


-- | The empty list as a top-level closure.
--
-- @
-- nil : [a]
-- @
nil = [program| nil = \ -> Nil |]

-- | Concatenate two lists. Haskell's @(++)@.
--
-- @
-- concat2 : [a] -> [a] -> [a]
-- @
concat2 = [program|
    concat2 = \xs ys -> case xs of
        Nil -> ys;
        Cons x xs' ->
            let rest = \(xs' ys) => concat2 xs' ys
            in Cons x rest;
        badList -> Error_concat2 badList
    |]

-- | Lazy left list fold. Provided mostly for seeing how it causes stack
-- overflows.
--
-- @
-- foldl : (b -> a -> b) -> b -> [a] -> b
-- @
foldl = [program|
    foldl = \f acc xs -> case xs of
        Nil -> acc;
        Cons y ys ->
            let acc' = \(f acc y) => case f acc y of v -> v
            in foldl f acc' ys;
        badList -> Error_foldl badList
    |]

-- | Strict left list fold.
--
-- Careful: the STG only supports primitive and algebraic case scrutinees.
-- As a result, you can only hand primitive or algebraic @b@ values to this
-- function or it will fail!
--
-- @
-- foldl' : (b -> a -> b) -> b -> [a] -> b
-- @
foldl' = [program|
    foldl' = \f acc xs -> case xs of
        Nil -> acc;
        Cons y ys -> case f acc y of
            acc' -> foldl' f acc' ys;
        badList -> Error_foldl' badList
    |]

-- | Right list fold.
--
-- @
-- foldr : (a -> b -> b) -> b -> [a] -> b
-- @
foldr = [program|
    foldr = \f z xs -> case xs of
        Nil -> z;
        Cons y ys ->
            -- rest only used once, no need for update
            let rest = \(f z ys) -> foldr f z ys
            in f y rest;
        badList -> Error_foldr badList
    |]

-- | Build a list by repeatedly applying a function to an initial value.
--
-- @
-- iterate f x = [x, f x, f (f x), ...]
-- @
--
-- @
-- iterate : (a -> a) -> a -> [a]
-- @
iterate = [program|
    iterate = \f x ->
        letrec
            fx = \(f x) => f x;
            rest = \(f fx) => iterate f fx
        in Cons x rest
    |]

-- | Infinite list created by repeating an initial (non-empty) list.
--
-- @
-- cycle [x,y,z] = [x,y,z, x,y,z, x,y,z, ...]
-- @
--
-- @
-- cycle : [a] -> [a]
-- @
cycle = concat2 <> [program|
    cycle = \xs ->
        letrec xs' = \(xs xs') => concat2 xs xs'
        in xs'
    |]

-- | Take n elements form the beginning of a list.
--
-- @
-- take 3 [1..] = [1,2,3]
-- @
--
-- @
-- take : Int -> [a] -> [a]
-- @
take = [program|
    take = \n ->
        letrec
            takePrim = \(takePrim) nPrim xs ->
                case nPrim of
                    0# -> Nil;
                    default -> case xs of
                        Nil -> Nil;
                        Cons x xs ->
                            let rest = \(takePrim xs nPrim) => case -# nPrim 1# of
                                    nPrimPred -> takePrim nPrimPred xs
                            in Cons x rest;
                        badList -> Error_take_badList badList
        in case n of
            Int# nPrim -> takePrim nPrim;
            badInt -> Error_take_badInt badInt
    |]

-- | Keep only the elements for which a predicate holds.
--
-- @
-- filter even [1..] = [2, 4, 6, ...]
-- @
--
-- @
-- filter : (a -> Bool) -> [a] -> [a]
-- @
filter = [program|
    filter = \p xs -> case xs of
        Nil -> Nil;
        Cons x xs' -> case p x of
            False -> filter p xs';
            True ->
                let rest = \(p xs') => filter p xs'
                in Cons x rest;
            badBool -> Error_filter_1 badBool;
        badList -> Error_filter_2 badList
    |]

-- | Separate a list into parts that do and do not satisfy a predicate.
--
-- @
-- partition even [1..6] = ([2,4,6], [1,3,5])
-- @
--
-- @
-- partition : (a -> Bool) -> [a] -> ([a], [a])
-- @
partition = nil <> [program|
    partition = \p xs -> case xs of
        Nil -> Pair nil nil;
        Cons y ys -> case partition p ys of
            Pair yes no -> case p y of
                True  -> let yes' = \(y yes) -> Cons y yes
                         in Pair yes' no;
                False -> let no' = \(y no) -> Cons y no
                         in Pair yes no';
                badBool -> Error_partition1 badBool;
            badPair -> Error_partition2 badPair;
        badList -> Error_partition3 badList
    |]

-- | reverse a list.
--
-- @
-- reverse [1,2,3] = [3,2,1]
-- @
--
-- @
-- reverse : [a] -> [a]
-- @
reverse = nil <> [program|
    reverse = \xs ->
        letrec
            reverse' = \(reverse') xs ys ->
                case xs of
                    Nil -> ys;
                    Cons x xs ->
                        let yxs = \(x ys) -> Cons x ys
                        in reverse' xs yxs;
                    badList -> Error_reverse badList
        in reverse' xs nil
    |]

-- | Repeat a single element infinitely.
--
-- @
-- repeat 1 = [1, 1, 1, ...]
-- @
--
-- @
-- repeat : a -> [a]
-- @
repeat = [program|
    repeat = \x ->
        letrec xs = \(x xs) -> Cons x xs
        in xs
    |]

-- | Repeat a single element a number of times.
--
-- @
-- replicate 3 1 = [1, 1, 1]
-- @
--
-- @
-- replicate : Int -> a -> [a]
-- @
replicate = [program|
    replicate = \n x ->
        letrec
            replicateXPrim = \(replicateXPrim x) nPrim ->
                case ># nPrim 0# of
                    0# -> Nil;
                    default ->
                        let rest = \(replicateXPrim nPrim) =>
                                case -# nPrim 1# of
                                    nPrimPred -> replicateXPrim nPrimPred
                        in Cons x rest
        in case n of
            Int# nPrim -> replicateXPrim nPrim;
            badInt -> Error_replicate badInt
    |]

-- | Haskell's Prelude sort function at the time of implementing this.
-- Not quite as pretty as the Haskell version, but functionally equivalent. :-)
--
-- This implementation is particularly efficient when the input contains runs of
-- already sorted elements. For comparison, sorting [1..100] takes 6496 steps,
-- whereas 'naiveSort' requires 268082.
--
-- @
-- sort : [Int] -> [Int]
-- @
sort = mconcat [gt_Int, Func.compose, nil] <> [program|
    sort = \ =>
        letrec
            sequences = \(descending ascending) xs2 -> case xs2 of
                Cons a xs1 -> case xs1 of
                    Cons b xs -> case gt_Int a b of
                        True -> let aList = \(a) -> Cons a nil
                                in descending b aList xs;
                        False -> let aCons = \(a) as -> Cons a as
                                 in ascending b aCons xs;
                        badBool -> Error_sort_sequences1 badBool;
                    Nil -> Cons xs2 nil;
                    badList -> Error_sort_sequences2 badList;
                Nil -> Cons xs2 nil;
                badList -> Error_sort_sequences3 badList;

            descending = \(descending sequences) a as bbs ->
                letrec
                    aas = \(a as) -> Cons a as;
                    fallthrough = \(sequences aas bbs) ->
                        let sequencesBs = \(sequences bbs) -> sequences bbs
                        in Cons aas sequencesBs
                in case bbs of
                    Cons b bs -> case gt_Int a b of
                        True -> descending b aas bs;
                        False -> fallthrough;
                        badBool -> Error_sort_descending1 badBool;
                    Nil -> fallthrough;
                    badList -> Error_sort_descending2 badList;

            ascending = \(ascending sequences) a as bbs ->
                letrec
                    asa = \(a as) ys ->
                        let aConsYs = \(a ys) -> Cons a ys
                        in as aConsYs;
                    fallthrough = \(sequences asa bbs) ->
                        let sequencesBs = \(sequences bbs) -> sequences bbs;
                            asaNil = \(asa) -> asa nil
                        in Cons asaNil sequencesBs
                in case bbs of
                    Cons b bs -> case gt_Int a b of
                        False -> ascending b asa bs;
                        True -> fallthrough;
                        badBool -> Error_sort_ascescending1 badBool;
                    Nil -> fallthrough;
                    badList -> Error_sort_ascescending2 badList;

            mergeAll = \(mergeAll mergePairs) xs -> case xs of
                Cons y ys -> case ys of
                    Nil -> y;
                    Cons _1 _2 -> compose mergeAll mergePairs xs;
                    badList -> Error_sort_mergeAll1 badList;
                Nil -> Error_sort_mergeAll_emptyListAsArgument;
                badList -> Error_sort_mergeAll2 badList;

            mergePairs = \(merge mergePairs) zs -> case zs of
                Cons a ys -> case ys of
                    Cons b xs ->
                        let mergeAB = \(merge a b) -> merge a b;
                            mergePairsXs = \(mergePairs xs) -> mergePairs xs
                        in Cons mergeAB mergePairsXs;
                    Nil -> zs;
                    badList -> Error_sort_mergePairs1 badList;
                Nil -> zs;
                badList -> Error_sort_mergePairs2 badList;

            merge = \(merge) as bs -> case as of
                Cons a as' -> case bs of
                    Cons b bs' -> case gt_Int a b of
                        True ->
                            let mergeAsBs' = \(merge as bs') => merge as bs'
                            in Cons b mergeAsBs';
                        False ->
                            let mergeAs'Bs = \(merge as' bs) => merge as' bs
                            in Cons a mergeAs'Bs;
                        badBool -> Error_sort_merge3 badBool;
                    Nil -> as;
                    badList -> Error_sort_merge2 badList;
                Nil -> bs;
                badList -> Error_sort_merge1 badList

        in compose mergeAll sequences |]

-- | That Haskell sort function often misleadingly referred to as "quicksort".
--
-- @
-- naiveSort : [Int] -> [Int]
-- @
naiveSort = mconcat [leq_Int, gt_Int, partition, concat2] <> [program|
    naiveSort = \xs -> case xs of
        Nil -> Nil;
        Cons pivot xs' ->
            let leqPivot = \(pivot) y -> leq_Int y pivot
            in case partition leqPivot xs' of
                Pair leqPivotXs gtPivotXs ->
                    letrec
                        leqPivotSorted = \(leqPivotXs) => naiveSort leqPivotXs;
                        gtPivotSorted = \(gtPivotXs) => naiveSort gtPivotXs;
                        fromPivotOn = \(pivot gtPivotSorted) -> Cons pivot gtPivotSorted
                    in concat2 leqPivotSorted fromPivotOn;
                badPair -> Error_sort_badPair badPair;
        badList -> Error_sort_badList badList |]

-- | Apply a function to each element of a list.
--
-- @
-- map : (a -> b) -> [a] -> [b]
-- @
map = [program|
    map = \f list -> case list of
        Nil       -> Nil;
        Cons x xs -> let fx  = \(f x)  => f x;
                         fxs = \(f xs) => map f xs
                     in Cons fx fxs;
        badList -> Error_map badList
    |]

-- | Equality of lists of integers.
--
-- @
-- equals_List_Int : [Int] -> [Int] -> Bool
-- @
equals_List_Int = Num.eq_Int <> [program|
    equals_List_Int = \xs ys ->
        case xs of
            Nil -> case ys of
                Nil -> True;
                Cons y ys' -> False;
                badList -> Error_listEquals badList;
            Cons x xs' -> case ys of
                Nil -> False;
                Cons y ys' -> case eq_Int x y of
                    True  -> equals_List_Int xs' ys';
                    False -> False;
                    badBool -> Error_listEquals_1 badBool;
                badList -> Error_listEquals_2 badList;
            badList -> Error_listEquals_3 badList
    |]

-- | Length of a list.
--
-- @
-- length : [a] -> Int
-- @
length = [program|
    length = \ =>
        letrec
            length' = \(length') n xs -> case xs of
                Nil -> Int# n;
                Cons y ys -> case +# n 1# of
                    n' -> length' n' ys;
                badList -> Error_length badList
        in length' 0#
    |]

-- | Zip two lists into one. If one list is longer than the other ignore the
-- exceeding elements.
--
-- @
-- zip [1,2,3,4,5] [10,20,30] ==> [(1,10),(2,20),(3,30)]
--
-- zip xs ys = zipWith Pair xs ys
-- @
--
-- @
-- zip : [a] -> [b] -> [(a,b)]
-- @
zip = [program|
    zip = \xs ys -> case xs of
        Nil -> Nil;
        Cons x xs' -> case ys of
            Nil -> Nil;
            Cons y ys' ->
                let tup  = \(x y)     -> Pair x y;
                    rest = \(xs' ys') => zip xs' ys'
                in Cons tup rest;
            badList -> Error_zip badList;
        badList -> Error_zip badList
    |]

-- | Zip two lists into one using a user-specified combining function.
-- If one list is longer than the other ignore the exceeding elements.
--
-- @
-- zipWith (+) [1,2,3,4,5] [10,20,30] ==> [11,22,33]
--
-- zipWith f xs ys = map f (zip xs ys)
-- @
--
-- @
-- zipWith : (a -> b -> c) -> [a] -> [b] -> [c]
-- @
zipWith = [program|
    zipWith = \f xs ys -> case xs of
        Nil -> Nil;
        Cons x xs' -> case ys of
            Nil -> Nil;
            Cons y ys' ->
                let fxy = \(f x y) => f x y;
                    rest = \(f xs' ys') => zipWith f xs' ys'
                in Cons fxy  rest;
            badList -> Error_zipWith badList;
        badList -> Error_zipWith badList
    |]

-- | Force the spine of a list.
--
-- @
-- forceSpine :: [a] -> [a]
-- @
forceSpine = [program|
    forceSpine = \xs ->
        letrec
            force = \(force) ys -> case ys of
                Nil        -> Done;
                Cons _ ys' -> force ys';
                badList    -> Error_forceSpine badList
        in case force xs of default -> xs
    |]
