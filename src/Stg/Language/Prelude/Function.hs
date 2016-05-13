{-# LANGUAGE QuasiQuotes #-}

module Stg.Language.Prelude.Function (
    seq,
    id,
    const,
    compose,
    fix,
) where



import Prelude ()

import Stg.Language (Program)
import Stg.Parser



seq, id, const, compose, fix :: Program



-- | Finally I can define 'Prelude.seq' directly! :-)
--
-- Note that this function is less powerful than GHC's 'Prelude.seq', since  STG
-- does not have a rule to force functions, only expressions that reduce to an
-- algebraic or primitive value. This leads to the fact that STG's @seq@ is less
-- powerful than Haskell's, since in Haskell
--
-- @
-- seq (const ()) () = ()
-- @
--
-- whereas in the STG
--
-- @
-- constUnit = () \n (x) -> Unit ();
-- seq (constUnit, Unit) = ERROR
-- @
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
-- Const : a -> b -> a
-- @
const = [stg| const = () \n (x,y) -> x () |]

-- | Function composition.
--
-- @
-- compose : (b -> c) -> (a -> b) -> a -> c
-- @
compose = [stg|
    compose = () \n (f, g, x) ->
        let gx = (g,x) \n () -> g (x) -- Only used once, no need for \u
        in f (gx)
    |]

-- | The fixed point combinator.
--
-- @
-- fix : (a -> a) -> a
-- @
fix = [stg|
    fix = () \n (f) ->
        letrec x = (f, x) \u () -> f (x)
        in x ()
    |]
