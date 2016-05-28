{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | A collection of  example programs that might be interesting to look  at
-- during execution.
module Stg.ExamplePrograms (

    -- * Folds

        -- ** Sum a list via the usual 'foldl''
        foldl'Sum,

        -- * Sum of a list via 'foldl'' implemented with 'foldr'
        foldl'ViaFoldrSum,


    -- * Fibonacci

        -- ** Naive implementation (exponential time)
        fibonacciNaive,

        -- ** Improved implementation (linear time)
        fibonacciImproved,

        -- ** zipWith (+) solution
        fibonacciZipWith,


    -- * List concatenation

        -- | It is well-known that Haskell's (++) operator is linear if
        -- associated to the right, but quadratic when associated to the left.
        -- These two examples showcase the issue.

        -- ** Right-associated
        listConcatRightAssociated,

        -- ** Left-associated
        listConcatLeftAssociated,
) where



import           Data.Monoid
import           Stg.Language
import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude            as Stg

-- | Sum up a list of 'Integer's using
--
-- @
-- sum = 'foldl'' ('+') 0
-- @
--
-- where 'foldl'' is the usual function known from Haskell.
foldl'Sum :: [Integer] -> Program
foldl'Sum list = mconcat
    [ Stg.foldl'
    , Stg.add
    , Stg.int "zero" 0
    , Stg.listOfNumbers "list" list
    , [program|
        sum = \ -> foldl' add zero;
        main = \ => sum list
    |]]

-- | Sum up a list of 'Integer's using
--
-- @
-- sum = 'foldl'' ('+') 0
-- @
--
-- where 'foldl'' is implemented via 'foldr' as
--
-- @
-- foldl' f z ys = foldr (\x xs acc -> xs $! f acc x) id ys z
-- @
--
-- which is a standard "'foldl'' in terms of 'foldr'" definition. This
-- definition is computationally equivalent to the standard 'foldl'', but has
-- more overhead.
foldl'ViaFoldrSum :: [Integer] -> Program
foldl'ViaFoldrSum list = mconcat
    [ foldl'Sum list
    , Stg.id
    , Stg.foldr
    , [program|
    foldl' = \f z xs ->
        let go = \(f) x xs acc -> case f acc x of
                forced -> xs forced
        in foldr go id xs z
    |]]



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
    , Stg.int "zero" 0
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
    , Stg.int "one" 1
    , Stg.int "n" n
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
    , Stg.int "zero" 1
    , Stg.int "one" 1
    , Stg.int "n" n
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
    [ Stg.listOfNumbers "list0" [0]
    , Stg.listOfNumbers "list1" [1]
    , Stg.listOfNumbers "list2" [2]
    , Stg.listOfNumbers "list3" [3]
    , Stg.listOfNumbers "list4" [4]
    , Stg.listOfNumbers "list5" [5]
    , Stg.listOfNumbers "list6" [6]
    , Stg.listOfNumbers "list7" [7]
    , Stg.listOfNumbers "list8" [8]
    , Stg.listOfNumbers "list9" [9]
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
