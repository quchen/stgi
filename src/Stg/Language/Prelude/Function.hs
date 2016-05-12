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
seq = [stgProgram| seq = () \n (x,y) -> case x () of default -> y () |]

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
fix = [stgProgram|
    fix = () \n (f) ->
        letrec x = (f, x) \u () -> f (x)
        in x ()
    |]
