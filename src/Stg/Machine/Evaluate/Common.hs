{-# LANGUAGE LambdaCase #-}

-- | Definitions useful for all state transition modules.
module Stg.Machine.Evaluate.Common (

    -- * Primops
    PrimError(..),
    applyPrimOp,

    -- * Algebraic matching
    AltMatch(..),
    AltError(..),
    lookupAlgebraicAlt,

    -- * Primitive matching
    lookupPrimitiveAlt,

) where



import qualified Data.List as L

import Stg.Language
import Stg.Util



-- | Possible errors of primops
data PrimError = Div0 -- ^ Division by zero

-- | Apply a primop to two actual integers
applyPrimOp :: PrimOp -> Integer -> Integer -> Validate PrimError Integer
applyPrimOp Div _ 0 = Failure Div0
applyPrimOp Mod _ 0 = Failure Div0
applyPrimOp op x y = Success (opToFunc op x y)
  where
    boolToPrim p a b = if p a b then 1 else 0
    opToFunc = \case
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> div
        Mod -> mod
        Eq  -> boolToPrim (==)
        Lt  -> boolToPrim (<)
        Leq -> boolToPrim (<=)
        Gt  -> boolToPrim (>)
        Geq -> boolToPrim (>=)
        Neq -> boolToPrim (/=)




-- | Successful alternative match, used for finding the right branch in @case@
data AltMatch alt = AltMatches alt | DefaultMatches DefaultAlt

-- | Possible errors when looking up alternatives
data AltError = BadAlt -- ^ Algebraic\/primitive alternative in
                       -- primitive\/algebraic case

-- | Look up an algebraic constructor among the given alternatives, and return
-- the first match. If nothing matches, return the default alternative.
lookupAlgebraicAlt
    :: Alts
    -> Constr
    -> Validate AltError (AltMatch AlgebraicAlt)
lookupAlgebraicAlt (Alts (AlgebraicAlts alts) def) constr
  = let matchingAlt (AlgebraicAlt c _ _) = c == constr
    in Success (case L.find matchingAlt alts of
        Just alt   -> AltMatches alt
        _otherwise -> DefaultMatches def )
lookupAlgebraicAlt (Alts PrimitiveAlts{} _) _ = Failure BadAlt
lookupAlgebraicAlt (Alts NoNonDefaultAlts{} def) _ = Success (DefaultMatches def)



-- | 'lookupAlgebraicAlt' for primitive literals.
lookupPrimitiveAlt
    :: Alts
    -> Literal
    -> Validate AltError (AltMatch PrimitiveAlt)
lookupPrimitiveAlt (Alts (PrimitiveAlts alts) def) lit
  = let matchingAlt (PrimitiveAlt lit' _) = lit' == lit
    in Success (case L.find matchingAlt alts of
        Just alt   -> AltMatches alt
        _otherwise -> DefaultMatches def )
lookupPrimitiveAlt (Alts AlgebraicAlts{} _) _ = Failure BadAlt
lookupPrimitiveAlt (Alts NoNonDefaultAlts{} def) _ = Success (DefaultMatches def)
