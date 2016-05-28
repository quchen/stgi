{-# LANGUAGE QuasiQuotes #-}

module Stg.Prelude.Function (
    seq,
    id,
    const,
    compose,
    fix,
) where



import Prelude ()

import Stg.Language           (Program)
import Stg.Parser.QuasiQuoter



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
-- constUnit = \(x) -> Unit ()
-- seq (constUnit, Unit) = ERROR
-- @
seq = [program| seq = \x y -> case x of default -> y |]

-- | Identity function.
--
-- @
-- id : a -> a
-- @
id = [program| id = \x -> x |]

-- | Constant function.
--
-- @
-- Const : a -> b -> a
-- @
const = [program| const = \x y -> x |]

-- | Function composition.
--
-- @
-- compose : (b -> c) -> (a -> b) -> a -> c
-- @
compose = [program|
    compose = \f g x ->
        let gx = \(g x) -> g x
        in f gx
    |]

-- | The fixed point combinator.
--
-- @
-- fix : (a -> a) -> a
-- @
fix = [program| fix = \f -> letrec x = \(f x) => f x in x |]
