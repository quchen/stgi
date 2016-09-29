{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | A collection of  example programs that might be interesting to look  at
-- during execution.
module Stg.ExamplePrograms (

    -- * Simple introductory programs

        implies,
        addTwoNumbers,
        calculateLength,
        mapNot,
        mapNotForced,

    -- * Sum of list

        -- ** via 'Data.Foldable.foldl''
        sum_foldl',

        -- ** via 'Data.Foldable.foldl'' implemented with 'foldr'
        sum_foldl'ViaFoldr,

        -- ** via 'foldl'
        sum_foldl,

        -- ** via 'foldr'
        sum_foldr,


    -- * Fibonacci

        -- ** Naive implementation (exponential time)
        fibonacciNaive,

        -- ** Improved implementation (linear time)
        fibonacciImproved,

        -- ** Infinite list with zipWith (+)
        fibonacciZipWith,


    -- * List concatenation

        -- | It is well-known that Haskell's (++) operator is linear if
        -- associated to the right, but quadratic when associated to the left.
        -- These two examples showcase the issue.

        -- ** Right-associated
        listConcatRightAssociated,

        -- ** Left-associated
        listConcatLeftAssociated,

    -- * Sorting
    naiveSort,
    librarySort,

    -- * Sharing

        -- ** Repeat
        repeatNaive,
        repeatSharing,
) where



import           Data.Monoid
import           Stg.Language
import           Stg.Marshal
import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude            as Stg





-- | A program that calculates @x and not y@. This is probably the simplest
-- example program worth showing. It demonstrates how functions are evaluated,
-- has an update for the @main@ closure, and shows how nested functions result
-- in @let@ allocations.
implies :: Bool -> Bool -> Program
implies p q = mconcat
    [ Stg.or2
    , Stg.not
    , toStg "p" p
    , toStg "q" q
    , [program|
    main = \ => let notP = \ -> not p
                in or2 notP q
    |]]

-- | A program that adds two numbers.
addTwoNumbers :: Integer -> Integer -> Program
addTwoNumbers x y = mconcat
    [ Stg.add
    , toStg "x" x
    , toStg "y" y
    , [program|
    main = \ => add x y
    |]]

-- | A program that measures the length of a list.
calculateLength :: ToStg a => [a] -> Program
calculateLength xs = mconcat
    [ Stg.length
    , toStg "xs" xs
    , [program|
    main = \ => length xs
    |]]

-- | Negate a list of booleans, but non-strictly. This program does almost
-- nothing, because nothing forces the list.
mapNot :: [Bool] -> Program
mapNot xs = mconcat
    [ Stg.not
    , Stg.map
    , toStg "xs" xs
    , [program|
    main = \ => map not xs
    |]]

-- | Negate a list of booleans strictly.
mapNotForced :: [Bool] -> Program
mapNotForced xs = mconcat
    [ Stg.not
    , Stg.map
    , toStg "xs" xs
    , [program|
    main = \ => let mapped = \ => map not xs
                in force mapped;
    force = \bs -> case bs of
        Nil -> Nil;
        Cons y ys -> case y of
            someBool -> case force ys of
                someList -> Cons someBool someList;
        badList -> error_force badList
    |]]



-- | Program to sum up a list, but with the @sum@ function left undefined.
sumTemplate :: [Integer] -> Program
sumTemplate list = mconcat
    [ Stg.add
    , toStg "zero" (0 :: Integer)
    , toStg "list" list
    , [program| main = \ => sum list |]]

-- | Sum up a list of 'Integer's using
--
-- @
-- sum = 'Data.Foldable.foldl'' ('+') 0
-- @
--
-- This is a good way to sum up a list in Haskell, as it runs in constant space.
sum_foldl' :: [Integer] -> Program
sum_foldl' list = mconcat
    [ sumTemplate list
    , Stg.foldl'
    , [program| sum = \ -> foldl' add zero |]]

-- | Sum up a list of 'Integer's using
--
-- @
-- sum = 'Data.Foldable.foldl'' ('+') 0
-- @
--
-- where 'Data.Foldable.foldl'' is implemented via 'foldr' as
--
-- @
-- foldl' f z ys = 'foldr' (\x xs acc -> xs '$!' f acc x) id ys z
-- @
--
-- which is a standard "'Data.Foldable.foldl'' in terms of 'foldr'" definition.
-- This definition is denotationally equivalent to the standard
-- 'Data.Foldable.foldl'', but has a bit more computational overhead.
sum_foldl'ViaFoldr :: [Integer] -> Program
sum_foldl'ViaFoldr list = mconcat
    [ sumTemplate list
    , Stg.id
    , Stg.foldr
    , [program|
    sum = \ -> foldl' add zero;
    foldl' = \f z xs ->
        let go = \(f) x xs acc -> case f acc x of
                forced -> xs forced
        in foldr go id xs z
    |]]

-- | Sum up a list of 'Integer's using
--
-- @
-- sum = 'foldl' ('+') 0
-- @
--
-- This is the canonical space leak in Haskell: note how the accumulator is
-- lazy, resulting in a large thunk buildup of suspended additions, that is only
-- collapsed to a final value after 'foldl' has terminated. The thunks are
-- stored on the heap, so it grows linearly with the length of the list. When
-- that thunk is forced, it will push lots of additions on the stack; in
-- summary, this produces a heap overflow, and if the heap is not exhausted, it
-- will try to overflow the stack.
sum_foldl :: [Integer] -> Program
sum_foldl list = mconcat
    [ sumTemplate list
    , Stg.foldl
    , [program| sum = \ -> foldl add zero |]]

-- | Sum up a list of 'Integer's using
--
-- @
-- sum = 'foldr' ('+') 0
-- @
--
-- Like the 'foldl' version demonstrated in 'sum_foldl', this is a space-leaking
-- implementation of the sum of a list. In this case however, the leak spills to
-- the stack and the heap alike: the stack contains the continuations for the
-- additions, while the heap contains thunks for the recursive call to @foldr@.
sum_foldr :: [Integer] -> Program
sum_foldr list = mconcat
    [ sumTemplate list
    , Stg.foldr
    , [program| sum = \ -> foldr add zero |]]



-- | Compute the list of Fibonacci numbers eagerly in the contents, but lazy in
-- the spine.
--
-- This means that the program will successively generate all the Fibonacci
-- numbers, allocating new cells of the infinite list and calculating their new
-- values, and garbage collecting previous values.
--
-- You can picture this as what happens to `fibo` in the Haskell program
--
-- @
-- main = let fibo = 'zipWith' ('+') fibo ('tail' fibo)
--        in 'Data.Foldable.traverse_' 'print' fibo
-- @
fibonacciZipWith :: Program
fibonacciZipWith = mconcat
    [ Stg.add
    , toStg "zero" (0 :: Integer)
    , Stg.foldl'
    , Stg.zipWith
    , [program|

    flipConst = \x y -> y;
    main = \ =>
        letrec
            fibo = \ =>
                letrec
                    fib0 = \(fib1) -> Cons zero fib1;
                    fib1 = \(fib2) =>
                        let one = \ -> Int# 1#
                        in Cons one fib2;
                    fib2 = \(fib0 fib1) => zipWith add fib0 fib1
                in fib0
        in foldl' flipConst zero fibo
    |]]

-- | Calculate the n-th Fibonacci number using the computationally (horribly)
-- inefficient formula
--
-- @
-- fib n | n <= 1 = n
-- fib n = fib (n-1) + fib (n-2)
-- @
--
-- This implementation is stack-only, so enjoy watching it explode. At the time
-- of writing this, the machine takes:
--
-- * fib  0  =>    27 steps
-- * fib  1  =>    27 steps
-- * fib  2  =>   122 steps
-- * fib  3  =>   218 steps
-- * fib  4  =>   410 steps
-- * fib  5  =>   698 steps
-- * fib  6  =>  1178 steps
-- * fib  7  =>  1946 steps
-- * fib  8  =>  3194 steps
-- * fib  9  =>  5210 steps
-- * fib 10  =>  8474 steps
fibonacciNaive :: Integer -> Program
fibonacciNaive n = mconcat
    [ Stg.add
    , Stg.leq_Int
    , Stg.sub
    , toStg "one" (1 :: Integer)
    , toStg "n" n
    , [program|
    main = \ =>
        letrec
            fib = \(fib) n -> case leq_Int n one of
            True -> n;
            _False -> case sub n one of
                nMinusOne -> case fib nMinusOne of
                    fibNMinusOne -> case sub nMinusOne one of
                        nMinusTwo -> case fib nMinusTwo of
                            fibNMinusTwo -> case add fibNMinusOne fibNMinusTwo of
                                result -> result
        in fib n
    |]]

-- | Calculate the n-th Fibonacci number using the more efficient formula
--
-- @
-- fib = fib' 0 1
--   where
--     fib' x _ | n <= 0 = x
--     fib' x !y n = fib' y (x+y) (n-1)
-- @
--
-- This implementation is a lot faster than the naive exponential
-- implementation. For example, calculating the 10th Fibonacci number (55) takes
-- only 490 steps, compared to the many thousands of the exponential version.
fibonacciImproved :: Integer -> Program
fibonacciImproved n = mconcat
    [ Stg.add
    , Stg.leq_Int
    , Stg.sub
    , toStg "zero" (0 :: Integer)
    , toStg "one" (1 :: Integer)
    , toStg "n" n
    , [program|
    main = \ =>
        letrec
            fib = \(fib') -> fib' zero one;
            fib' = \(fib') x y n -> case leq_Int n zero of
                True -> x;
                _False -> case add x y of
                    xy -> case sub n one of
                        nMinusOne -> fib' y xy nMinusOne
        in fib n
    |]]

-- | List concatenation example with the 'concat' definition left out.
listConcatTemplate :: ToStg a => [[a]] -> Program
listConcatTemplate xss = mconcat
    [ toStg "xss" xss
    , Stg.concat2
    , [program|

    forceList = \xs -> case xs of
        Nil -> Done;
        Cons _ xs' -> forceList xs';
        _ -> BadListError;

    concatenated = \ => concat xss;
    main = \ => case forceList concatenated of
        _ -> concatenated

    |]]

-- | Force a right-associated concatenation
--
-- @
-- [0] '++' ([1] '++' ([2] '++' ([3])))
-- @
--
-- and store it in the @main@ closure.
--
-- This computation is __linear__ in the number of elements of the sublists.
listConcatRightAssociated :: ToStg a => [[a]] -> Program
listConcatRightAssociated xss = mconcat
    [ listConcatTemplate xss
    , Stg.foldr
    , [program| concat = \ -> foldr concat2 nil |]]

-- | Force a left-associated concatenation
--
-- @
-- (([0] '++' [1]) '++' [2]) '++' [3]
-- @
--
-- and store it in the @main@ closure.
--
-- This computation is __quadratic__ in the number of elements of the sublists.
listConcatLeftAssociated :: ToStg a => [[a]] -> Program
listConcatLeftAssociated xss = mconcat
    [ listConcatTemplate xss
    , Stg.foldl'
    , [program| concat = \ -> foldl' concat2 nil |]]

-- | Sort a list with the canonical Quicksort-inspired algorithm often found
-- in introductory texts about Haskell.
--
-- Note that this is not Quicksort itself, as one key feature of it is sorting
-- in-place. In particular, this algorithm is not all that quick, as it takes
-- almost a thousand steps to reach the final state when sorting @[5,4,3,2,1]@.
naiveSort :: [Integer] -> Program
naiveSort xs =
    toStg "xs" xs
    <> Stg.forceSpine
    <> Stg.naiveSort
    <> [program|
        sorted = \ => naiveSort xs;
        main = \ => forceSpine sorted |]

-- | Sort a list with a translation of Haskell's 'Data.List.sort', which is
-- an implementation of mergesort with ordered sublist detection.
librarySort :: [Integer] -> Program
librarySort xs =
    toStg "xs" xs
    <> Stg.forceSpine
    <> Stg.sort
    <> [program|
        sorted = \ => sort xs;
        main = \ => forceSpine sorted |]



-- | This is a naive implementation of the 'repeat' function,
--
-- @
-- 'repeat' x = x : 'repeat' x
-- @
--
-- and it is used to compute the infinite repetition of a number. Run this
-- program for a couple hundred steps and observe the heap and the garbage
-- collector. Count the GC invocations, and compare it to the behaviour of
-- 'repeatSharing'! Also note how long it takes to generate two successive
-- list elements.
--
-- The reason for this behaviour is that the call to @'repeat' x@ is not shared,
-- but done again for each cons cell, requiring one heap allocation every time.
-- Cleaning up after this keeps the GC quite busy.
repeatNaive :: Program
repeatNaive = repeatSharing <> [program|
    repeat = \x ->
        let repeatX = \(x) -> repeat x
        in Cons x repeatX
    |]

-- | This uses a much better definition of 'repeat',
--
-- @
-- 'repeat' x = let repeatX = x : repeatX
--            in repeatX
-- @
--
-- This program does only a total of three heap allocations before continuously
-- running without interruption: one for the @repeated@ value, one for the
-- self-referencing cons cell, and one because of how 'Stg.forceSpine' works.
--
-- Note how much smaller the cycles between the traversal of two neighbouring
-- list cells are!
repeatSharing :: Program
repeatSharing = mconcat
    [ Stg.forceSpine
    , Stg.repeat
    , [program|
    main = \ =>
        let repeated = \ -> repeat 1#
        in case forceSpine repeated of v -> v
    |]]
