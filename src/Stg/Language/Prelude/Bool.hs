{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}

-- | Boolean functions, like in "Data.Bool".
module Stg.Language.Prelude.Bool (
    and2,
    or2,
    not,
    bool,
    eq_Bool,
    boolValue,
) where



import Prelude (Bool)

import Data.Text (Text)

import Stg.Language
import Stg.Parser
import Stg.Util



eq_Bool, and2, or2, not, bool :: Program

-- | Boolean equality.
eq_Bool = [stg|
    eq_Bool = () \n (x,y) -> case x () of
        True () -> case y () of
            True () -> True ();
            False () -> False ();
            badBool -> Error_eq_Bool (badBool);
        False () -> case y () of
            True () -> False ();
            False () -> True ();
            badBool -> Error_eq_Bool (badBool);
        badBool -> Error_eq_Bool (badBool)
    |]

-- | Binary and. Haskell's @(&&)@.
--
-- @
-- && : Bool -> Bool -> Bool
-- @
and2 = [stg|
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
or2 = [stg|
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
not = [stg|
    not = () \n (x) -> case x () of
        True ()  -> False ();
        False () -> True ();
        badBool  -> Error_not (badBool)
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
bool = [stg|
    bool = () \n (f,t,p) -> case p () of
        True () -> t ();
        False () -> f ();
        badBool -> Error_bool (badBool)
    |]

-- | Inject a boolean value into an STG program.
boolValue
    :: Text -- ^ Name
    -> Bool -- ^ Value
    -> Program
boolValue name b = Program (Binds [(Var name,LambdaForm [] NoUpdate [] (AppC (Constr (show' b)) []))])
