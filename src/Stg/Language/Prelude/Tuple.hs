{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Stg.Language.Prelude.Tuple (
    fst,
    snd,
    curry,
    uncurry,
    swap,

    -- * Convenience
    tupleOfNumbers,
    equals_Tuple_Int
) where



import           Data.Monoid
import qualified Data.Text                   as T
import qualified Prelude                     as P

import           Stg.Language
import           Stg.Language.Prelude.Number
import           Stg.Parser



fst, snd, curry, uncurry, swap :: Program
equals_Tuple_Int :: Program



-- | First element of a tuple.
--
-- @
-- fst : (a,b) -> a
-- @
fst = [stg|
    fst = () \n (tuple) ->
        case tuple () of
            Tuple (a,b) -> a ();
            badTuple -> Error_fst (badTuple) |]

-- | Second element of a tuple.
--
-- @
-- snd : (a,b) -> a
-- @
snd = [stg|
    snd = () \n (tuple) ->
        case tuple () of
            Tuple (a,b) -> b ();
            badTuple -> Error_snd (badTuple) |]

-- | Convert an uncurried function to a curried one.
--
-- @
-- curry : ((a, b) -> c) -> a -> b -> c
-- @
curry = [stg|
    curry = () \n (f, x, y) ->
        let tuple = (x,y) \u () -> Tuple (x,y)
        in f (tuple) |]

-- | Convert a curried function to an uncurried one.
--
-- @
-- uncurry : (a -> b -> c) -> (a, b) -> c
-- @
uncurry = fst <> snd <> [stg|
    uncurry = () \n (f, tuple) ->
        let fst' = (tuple) \n () -> fst (tuple);
            snd' = (tuple) \n () -> snd (tuple)
        in f (fst', snd') |]

-- | Swap the elements of a tuple.
--
-- @
-- swap : (a,b) -> (b,a)
-- @
swap = [stg|
    swap = () \n (tuple) ->
        case tuple () of
            Tuple (a,b) -> Tuple (b,a);
            badTuple -> Error_snd (badTuple) |]

-- | Generate a tuple of numbers.
--
-- @
-- tupleOfNumbers (1,2)
-- @
tupleOfNumbers
    :: T.Text                 -- ^ Name of the tuple in the STG program
    -> (P.Integer, P.Integer) -- ^ Entries
    -> Program
tupleOfNumbers name (x,y) =
    Program (Binds [(Var name, LambdaForm [] Update []
        (Let NonRecursive
            (Binds
                [ (Var "x", LambdaForm [] NoUpdate []
                    (AppC (Constr "Int#") [AtomLit (Literal x)]))
                ,(Var "y", LambdaForm [] NoUpdate []
                    (AppC (Constr "Int#") [AtomLit (Literal y)]))])
            (AppC (Constr "Tuple") [AtomVar (Var "x"),AtomVar (Var "y")])))])


equals_Tuple_Int = eq <> [stg|
    eq_Tuple_Int = () \n (tup1, tup2) ->
        case tup1 () of
            Tuple (a,b) -> case tup2 () of
                Tuple (x,y) -> case eq_Int (a,x) of
                    True () -> case eq_Int (b,y) of
                        True () -> True ();
                        False () -> False ();
                        badBool -> Error_eq_Tuple (badBool);
                    False () -> False ();
                    badBool -> Error_eq_Tuple (badBool);
                badTuple -> Error_eq_Tuple (badTuple);
            badTuple -> Error_eq_Tuple (badTuple)
    |]
