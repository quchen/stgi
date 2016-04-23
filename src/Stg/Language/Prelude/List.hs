{-# LANGUAGE QuasiQuotes #-}

module Stg.Language.Prelude.List (
    concat,
    foldl,
    foldl',
    foldr,
    iterate,
    cycle,
    take,
    filter,
    repeat,
) where



import           Prelude                     ()

import           Data.Monoid

import           Stg.Language                (Program)
import           Stg.Parser

import           Stg.Language.Prelude.Number as Num



concat, foldl, foldl', foldr, iterate, cycle, take, filter, repeat :: Program

concat = [stg|
    concat = () \n (xs,ys) -> case xs () of
        Nil () -> ys ();
        Cons (x,xs') ->
            let rest = (xs', ys) \u () -> concat (xs', ys)
            in Cons (x, rest);
        def -> Error_concat (def)
    |]

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
            let rest = (f,z,ys) \u () -> foldr (f,z,ys)
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
iterate = [stg|
    iterate = () \n (f,x) ->
        letrec fx = (f,x) \u () -> f (x);
               rest = (f,fx) \u () -> iterate (f,fx)
        in Cons (x,rest) |]

-- | Infinite list, created by repeating an initial (non-empty) list.
--
-- @
-- cycle [x,y,z] = [x,y,z, x,y,z, x,y,z, ...]
-- @
--
-- @
-- cycle : [a] -> [a]
-- @
cycle = concat <> [stg|
    cycle = () \n (xs) ->
        letrec xs' = (xs, xs') \u () -> concat (xs, xs')
        in xs' ()
    |]

-- | Take n elements form the beginning of a list.
--
-- @
-- take : Int -> [a] -> [a]
-- @
take = Num.add <> [stg|
    take = () \u () ->
        letrec  minusOne = () \n () -> Int# (-1#);
                take' = (minusOne) \n (n, xs) -> case n () of
                    Int# (nPrim) -> case nPrim () of
                        0# -> Nil ();
                        default ->
                            let n' = (n, minusOne) \u () -> add (n, minusOne)
                            in case xs () of
                               Nil () -> Nil ();
                               Cons (y,ys) ->
                                   let rest = (n', ys) \u () -> take (n', ys)
                                   in Cons (y, rest)
                               default -> Error_take_not_a_list ()
                    default -> Error_take_not_an_int ()
        in take' ()
    |]

filter = [stg|
    filter = () \n (p, xs) -> case xs () of
        Nil () -> Nil ();
        Cons (x,xs') -> case p (x) of
            False () -> filter (p, xs');
            True () ->
                let rest = (p, xs') \u () -> filter (p, xs')
                in Cons (x, rest);
            def -> Error_filter (def)
        def -> Error_filter (def)
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
repeat = [stg|
    repeat = () \n (x) ->
        letrec xs = (x, xs) \u () -> Cons (x,xs)
        in xs ()
    |]
