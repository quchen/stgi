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
    concat,
    foldl,
    foldl',
    foldr,
    iterate,
    cycle,
    take,
    repeat,

    -- * Numbers
    add,
    numbers,

    -- * Other
    seq,
    id,
    const,
    compose,
) where



import           Prelude      ()

import           Data.Monoid

import           Stg.Language (Program)
import           Stg.Parser



listC, concat, foldl, foldl', foldr, iterate, cycle, take, repeat :: Program
numbers, add :: Program
seq, id, const, compose :: Program

-- | List standard constructors.
--
-- @
-- nil : [a]
-- cons : (a, [a]) -> [a]
-- @
listC = [stg|
    nil = () \n () -> Nil ();
    cons = () \n (x,xs) -> Cons (x,xs) |]

concat = [stg|
    concat = () \n (xs, ys) -> case xs () of
        Nil () -> ys ();
        Cons (x,xs') -> let rest = () \u () -> concat (xs', ys)
                        in cons (x, rest)
        default -> Error_concat ()
    |]

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
iterate = listC <> [stg|
    iterate = () \n (f,x) ->
        letrec fx = (f,x) \u () -> f (x);
               rest = (f,fx) \u () -> iterate (f,fx)
        in cons (x,rest) |]

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
take = listC <> add <> [stg|
    take = () \u () ->
        letrec  minusOne = () \n () -> Int# (-1#);
                take' = (minusOne) \n (n, xs) -> case n () of
                    Int# (nPrim) -> case nPrim () of
                        0# -> nil ();
                        default ->
                            let n' = (n, minusOne) \u () -> add (n, minusOne)
                            in case xs () of
                               Nil () -> nil ();
                               Cons (y,ys) ->
                                   let rest = (n', ys) \u () -> take (n', ys)
                                   in cons (y, rest)
                               default -> Error_take_not_a_list ()
                    default -> Error_take_not_an_int ()
        in take' ()
    |]

-- TODO doc
-- filter = listC <> [stg|
--     filter = () \n (p) ->
--     |]

-- | Repeat a single element infinitely.
--
-- @
-- repeat : a -> [a]
-- @
repeat = listC <> [stg|
    repeat = () \n (x) ->
        letrec xs = (x, xs) \u () -> cons (x,xs)
        in xs ()
    |]


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
numbers = [stg|
    -- minusOne = () \n () -> Int# (-1#);
    zero     = () \n () -> Int# (0#);
    one      = () \n () -> Int# (1#);
    two      = () \n () -> Int# (2#);
    three    = () \n () -> Int# (3#);
    ten      = () \n () -> Int# (10#) |]



-- | Finally I can define 'Prelude.seq' directly! :-)
seq = [stg| seq = () \n (x,y) -> case x () of default -> y () |]

-- | Identity function.
--
-- @
-- id : a -> a
-- @
id = [stg| id = () \n (x) -> x () |]

-- | Constant function.
--
-- @
-- const : (a, b) -> a
-- @
const = [stg| const = () \n (x,y) -> x () |]

-- | Function composition.
--
-- @
-- compose : (b -> c, a -> b) -> a -> c
-- @
compose = [stg|
    compose = () \n (f, g) ->
        let fgx = (f,g) \n (x) ->
                let gx = (g,x) \n () -> g (x) -- TODO: \u or \n?
                in f (gx)
        in fgx ()
    |]
