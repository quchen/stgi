{-# LANGUAGE QuasiQuotes #-}

-- | Boolean functions, like in "Data.Bool".
module Stg.Language.Prelude.Bool (
    and2,
    or2,
    not,
    bool,
) where



import Prelude ()

import Stg.Language (Program)
import Stg.Parser



and2, or2, not, bool :: Program



-- | Binary and. Haskell's @(&&)@.
--
-- @
-- && : Bool -> Bool -> Bool
-- @
and2 = [stgProgram|
    and2 = () \n (x,y) -> case x () of
        True ()  -> y ();
        False () -> False ();
        badBool  -> Error_and2 (badBool)
    |]

-- | Binary or. Haskell's @(||)@.
--
-- @
-- || : Bool -> Bool -> Bool
-- @
or2 = [stgProgram|
    or2 = () \n (x,y) -> case x () of
        True ()  -> True ();
        False () -> y ();
        badBool  -> Error_or2 (badBool)
    |]

-- | Binary negation.
--
-- @
-- not : Bool -> Bool
-- @
not = [stgProgram|
    not = () \n (x) -> case x () of
        True ()  -> False ();
        False () -> True ();
        badBool  -> Error_not (badBool)
    |]


-- | Boolean deconstructor.
--
-- @
-- bool : a -> a -> Bool -> a
-- @
bool = [stgProgram|
    bool = () \n (t,f,p) -> case p () of
        True () -> t ();
        False () -> f ();
        badBool -> Error_bool (badBool)
    |]
