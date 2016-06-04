{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | A collection of  example programs that might be interesting to look  at
-- during execution.
module Stg.ExamplePrograms (

    -- * Simple introductory programs

        addTwoNumbers,
        calculateLength,

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





-- | A program that adds two numbers.
addTwoNumbers :: Integer -> Integer -> Program
addTwoNumbers x y = mconcat
    [ Stg.add
    , toStg "x" x
    , toStg "y" y
    , [program|
    main = \ => case add x y of r -> r
    |]]

-- | A program that measures the length of a list.
calculateLength :: [Integer] -> Program
calculateLength xs = mconcat
    [ Stg.length
    , toStg "xs" xs
    , [program|
    main = \ => case length xs of r -> r
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

-- | Calculate the n-th Fibonacci number using the more effective formula
--
-- @
-- fib = fib' 0 1
--   where
--     fib' x _ | n <= 0 = x
--     fib' x !y n = fib' y (x+y) (n-1)
-- @
--
-- This implementation is a lot faster than the naive exponential
-- implementation. For examle, calculating the 10th Fibonacci number (55) takes
-- only 490 steps, compared to the many thousand of the exponential version.
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

-- | Force the right-associated concatenation
--
-- @
-- [0] '++' ([1] '++' ([2] '++' ([3] '++' ([4] '++' ([5] '++' ([6] '++' ([7] '++' ([8] '++' [9]))))))))
-- @
--
-- and store it in the @main@ closure.
--
-- This computation is __linear__ in the number of elements of the sublists.
listConcatRightAssociated :: Program
listConcatRightAssociated = mconcat
    [ toStg "list0" [0 :: Integer]
    , toStg "list1" [1 :: Integer]
    , toStg "list2" [2 :: Integer]
    , toStg "list3" [3 :: Integer]
    , toStg "list4" [4 :: Integer]
    , toStg "list5" [5 :: Integer]
    , toStg "list6" [6 :: Integer]
    , toStg "list7" [7 :: Integer]
    , toStg "list8" [8 :: Integer]
    , toStg "list9" [9 :: Integer]
    , Stg.concat2
    , Stg.nil
    , [program|

    forceList = \xs -> case xs of
        Nil -> Done;
        Cons _ xs' -> forceList xs';
        _ -> BadListError;

    concatenated = \ =>
        letrec
            list0123456789 = \(list123456789) => concat2 list0 list123456789;
            list123456789  = \(list23456789)  => concat2 list1 list23456789;
            list23456789   = \(list3456789)   => concat2 list2 list3456789;
            list3456789    = \(list456789)    => concat2 list3 list456789;
            list456789     = \(list56789)     => concat2 list4 list56789;
            list56789      = \(list6789)      => concat2 list5 list6789;
            list6789       = \(list789)       => concat2 list6 list789;
            list789        = \(list89)        => concat2 list7 list89;
            list89         = \                => concat2 list8 list9
        in list0123456789;

    main = \ => case forceList concatenated of
        _ -> concatenated

    |]]

-- | Force the left-associated concatenation
--
-- @
-- (((((((([0] '++' [1]) '++' [2]) '++' [3]) '++' [4]) '++' [5]) '++' [6]) '++' [7]) '++' [8]) '++' [9]
-- @
--
-- and store it in the @main@ closure.
--
-- This computation is __quadratic__ in the number of elements of the sublists.
listConcatLeftAssociated :: Program
listConcatLeftAssociated = listConcatRightAssociated <> [program|
    concatenated = \ =>
        letrec
            list01         = \                => concat2 list0         list1;
            list012        = \(list01)        => concat2 list01        list2;
            list0123       = \(list012)       => concat2 list012       list3;
            list01234      = \(list0123)      => concat2 list0123      list4;
            list012345     = \(list01234)     => concat2 list01234     list5;
            list0123456    = \(list012345)    => concat2 list012345    list6;
            list01234567   = \(list0123456)   => concat2 list0123456   list7;
            list012345678  = \(list01234567)  => concat2 list01234567  list8;
            list0123456789 = \(list012345678) => concat2 list012345678 list9
        in list0123456789
    |]



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
-- This program does only a total of three heap allocations before continously
-- running without interruption: one for the @repeated@ value, one for the
-- self-referencing cons cell, and one beacuse of how 'Stg.forceSpine' works.
--
-- Note how much smaller the cycles between the traversal of two neighbouring
-- list cells is!
repeatSharing :: Program
repeatSharing = mconcat
    [ Stg.forceSpine
    , Stg.repeat
    , [program|
    main = \ =>
        let repeated = \ -> repeat 1#
        in case forceSpine repeated of v -> v
    |]]
