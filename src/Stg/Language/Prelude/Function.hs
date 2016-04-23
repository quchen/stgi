{-# LANGUAGE QuasiQuotes #-}

module Stg.Language.Prelude.Function (
    seq,
    id,
    const,
    compose,
) where



import           Prelude      ()

import           Stg.Language (Program)
import           Stg.Parser



seq, id, const, compose :: Program



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
    compose = () \n (f, g) ->
        let fgx = (f,g) \n (x) ->
                let gx = (g,x) \n () -> g (x) -- TODO: \u or \n?
                in f (gx)
        in fgx ()
    |]
