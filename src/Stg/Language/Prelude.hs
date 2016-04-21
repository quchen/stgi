{-# LANGUAGE QuasiQuotes #-}

-- | Common Haskell functions, translated to STG. Use the 'Monoid' instance
-- for 'Program' to mix them.
--
-- Each definition is written so that it can be used directly; for example,
-- 'iterate' includes the definition of 'cons', and you don't have to add it
-- manually.
--
-- This module should be imported qualified, since it heavily conflicts with the
-- standard Haskell "Prelude".
module Stg.Language.Prelude (
    -- * Lists
    listC,
    foldl,
    foldl',
    foldr,
    iterate,

    -- * Numbers
    add,
    numbers,

    -- * Other
    seq,
) where



import           Prelude      ()

import           Data.Monoid

import           Stg.Language (Program)
import           Stg.Parser



listC, foldl, foldl', foldr, iterate :: Program
numbers, add :: Program
seq :: Program

-- | List standard constructors.
--
-- @
-- nil : [a]
-- cons : (a, [a]) -> [a]
-- @
listC = [stg|
    nil = () \n () -> Nil ();
    cons = () \n (x,xs) -> Cons (x,xs) |]

-- | Integer addition.
--
-- @
-- add : (Int, Int) -> Int
-- @
add = [stg|
    add = () \n (x,y) -> case x () of
        Int# (x') -> case y () of
            Int# (y') -> case +# x' y' of
                1# -> Int# (1#); -- FIXME type hint
                v  -> Int# (v)
            default -> Error_add ()
        default -> Error_add () |]

-- | Lazy left list fold.
--
-- @
-- foldl : (b -> a -> b, b, [a]) -> b
-- @
foldl = [stg|
    foldl = () \n (f, acc, xs) -> case xs () of
        Nil () -> acc ();
        Cons (y,ys) ->
            let acc' = (f,acc,y) \u () -> case f (acc, y) of
                    v -> v ()
            in foldl (f, acc', ys)
        default -> Error_foldl () |]

-- | Strict left list fold.
--
-- @
-- foldl : (b -> a -> b, b, [a]) -> b
-- @
foldl' = [stg|
    foldl' = () \n (f, acc, xs) -> case xs () of
        Nil () -> acc ();
        Cons (y,ys) -> case f (acc, y) of
            acc' -> foldl' (f, acc', ys)
        default -> Error_foldl' ()
    |]

-- | Right list fold.
--
-- @
-- foldl : (a -> b -> b, b, [a]) -> b
-- @
foldr = [stg|
    foldr = () \n (f, z, xs) -> case xs () of
        Nil () -> z ();
        Cons (y,ys) ->
            let rest = () \n () -> foldr (f,z,ys)
            in f (y, rest)
        default -> Error_foldr () |]

-- | Build a list by repeatedly applying a function to an initial value.
--
-- @
-- iterate f x = [x, f x, f (f x), ...]
-- @
--
-- @
-- iterate : (a -> a, a) -> [a]
-- @
iterate = listC <> [stgProgram|
    iterate = () \n (f,x) ->
        letrec fx = (f,x) \u () -> f (x);
               rest = (f,fx) \u () -> iterate (f,fx)
        in cons (x,rest) |]

-- | Various common numbers.
--
-- @
-- minusOne = -1
-- zero     =  0
-- one      =  1
-- two      =  2
-- three    =  3
-- ten      = 10
-- @
numbers = [stgProgram|
    minusOne = () \n () -> Int# (-1#);
    zero     = () \n () -> Int# (0#);
    one      = () \n () -> Int# (1#);
    two      = () \n () -> Int# (2#);
    three    = () \n () -> Int# (3#);
    ten      = () \n () -> Int# (10#) |]

-- | Finally I can define 'Prelude.seq' directly! :-)
seq = [stgProgram| seq = () \n (x,y) -> case x () of default -> y () |]
