{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Stg.Prelude.Tuple (
    fst,
    snd,
    curry,
    uncurry,
    swap,

    equals_Pair_Int,
) where



import Prelude ()

import Data.Monoid

import Stg.Language
import Stg.Parser.QuasiQuoter
import Stg.Prelude.Number



fst, snd, curry, uncurry, swap :: Program
equals_Pair_Int :: Program



-- | First element of a tuple.
--
-- @
-- fst : (a,b) -> a
-- @
fst = [program|
    fst = \tuple ->
        case tuple of
            Pair a b -> a;
            badPair  -> Error_fst badPair
    |]

-- | Second element of a tuple.
--
-- @
-- snd : (a,b) -> a
-- @
snd = [program|
    snd = \tuple ->
        case tuple of
            Pair a b -> b;
            badPair  -> Error_snd badPair
    |]

-- | Convert an uncurried function to a curried one.
--
-- @
-- curry : ((a, b) -> c) -> a -> b -> c
-- @
curry = [program|
    curry = \f x y ->
        let tuple = \(x y) -> Pair x y
        in f tuple
    |]

-- | Convert a curried function to an uncurried one.
--
-- @
-- uncurry : (a -> b -> c) -> (a, b) -> c
-- @
uncurry = fst <> snd <> [program|
    uncurry = \f tuple ->
        let fst' = \(tuple) -> fst tuple;
            snd' = \(tuple) -> snd tuple
        in f fst' snd'
    |]

-- | Swap the elements of a tuple.
--
-- @
-- swap : (a,b) -> (b,a)
-- @
swap = [program|
    swap = \tuple ->
        case tuple of
            Pair a b -> Pair b a;
            badPair  -> Error_snd badPair |]


equals_Pair_Int = eq_Int <> [program|
    eq_Pair_Int = \tup1 tup2 ->
        case tup1 of
            Pair a b -> case tup2 of
                Pair x y -> case eq_Int a x of
                    True -> eq_Int b y;
                    False -> False;
                    badBool -> Error_eq_Pair badBool;
                badPair -> Error_eq_Pair badPair;
            badPair -> Error_eq_Pair badPair
    |]
