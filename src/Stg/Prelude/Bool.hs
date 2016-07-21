{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}

-- | Boolean functions, like in "Data.Bool".
--
-- This module should be imported qualified to avoid clashes with standard
-- Haskell definitions.
module Stg.Prelude.Bool (
    and2,
    or2,
    not,
    bool,
    eq_Bool,
) where



import Prelude ()

import Stg.Language
import Stg.Parser.QuasiQuoter



eq_Bool, and2, or2, not, bool :: Program

-- | Boolean equality.
eq_Bool = [program|
    eq_Bool = \x y -> case x of
        True -> case y of
            True    -> True;
            False   -> False;
            badBool -> Error_eq_Bool badBool;
        False -> case y of
            True    -> False;
            False   -> True;
            badBool -> Error_eq_Bool badBool;
        badBool -> Error_eq_Bool badBool
    |]

-- | Binary and. Haskell's @(&&)@.
--
-- @
-- && : Bool -> Bool -> Bool
-- @
and2 = [program|
    and2 = \x y -> case x of
        True  -> y;
        False -> False;
        badBool  -> Error_and2 badBool
    |]

-- | Binary or. Haskell's @(||)@.
--
-- @
-- || : Bool -> Bool -> Bool
-- @
or2 = [program|
    or2 = \x y -> case x of
        True     -> True;
        False    -> y;
        badBool  -> Error_or2 badBool
    |]

-- | Binary negation.
--
-- @
-- not : Bool -> Bool
-- @
not = [program|
    not = \x -> case x of
        True -> False;
        False -> True;
        badBool  -> Error_not badBool
    |]


-- | Boolean deconstructor.
--
-- @
-- bool f _ False = f
-- bool _ t True  = t
-- @
--
-- @
-- bool : a -> a -> Bool -> a
-- @
bool = [program|
    bool = \f t p -> case p of
        True    -> t;
        False   -> f;
        badBool -> Error_bool badBool
    |]
