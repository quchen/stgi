{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | A collection of  example programs that might be interesting to look  at
-- during execution.
module Stg.ExamplePrograms (

    -- * Fibonacci

        -- ** Naive implementation
        fibonacciNaive,

        -- ** zipWith (+) solution
        fibonacciZipWith,
) where



import           Stg.Language
import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude            as Stg



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
-- main = traverse_ print fibo
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
