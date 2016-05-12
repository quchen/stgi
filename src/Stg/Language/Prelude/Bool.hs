{-# LANGUAGE QuasiQuotes #-}

module Stg.Language.Prelude.Bool (
    and2,
    or2,
    not,
) where



import Prelude ()

import Stg.Language (Program)
import Stg.Parser



and2, or2, not :: Program



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
