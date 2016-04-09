{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines a dictionary to build prettyprinters with.
module Stg.Language.Prettyprint.Module (
    makePrettyprinter,
    modifyModule,
    PrettyprinterDict(..),
    PrettyprinterModule(..),
) where



import           Data.Function
import           Prelude       hiding ((<$>))

import           Stg.Language



-- | Create a 'PrettyprinterDict' from a 'PrettyprinterModule' by resolving
-- circular references.
makePrettyprinter :: PrettyprinterModule out -> PrettyprinterDict out
makePrettyprinter (PrettyprinterModule f) = fix f



-- | A module, i.e. potentially mutually recursive collection of,
-- functions to prettyprint to an output of type @out@.
newtype PrettyprinterModule out =
    PrettyprinterModule (PrettyprinterDict out -> PrettyprinterDict out)



-- | Modify a 'PrettyprinterModule' by mapping its contained
-- 'PrettyprinterDict' to an altered version.
--
-- For example, if you want to modify a default prettyprinter to omit the
-- update flag in output, use
--
-- @
-- modifyModule pprModule (\dict -> dict { pprRec = \rec _upd -> "" })
-- @
--
-- For a practical use of this, see 'parserInverseColouredModule'.
modifyModule
    :: PrettyprinterModule out
    -> (PrettyprinterDict out -> PrettyprinterDict out)
    -> PrettyprinterModule out
modifyModule pprModule modifier = PrettyprinterModule (\rec ->
    let (PrettyprinterModule pprKnotter) = pprModule
    in modifier (pprKnotter rec))



-- | Dictionary of prettyprinting functions.
--
-- This is similar to using type classes, but provides additional flexibility,
-- such as buidling new prettyprinters based on existing ones in order to
-- e.g. add colouring or ignore certain elements.
data PrettyprinterDict out = PrettyprinterDict
    { pprProgram       :: Program       -> out
    , pprBinds         :: Binds         -> out
    , pprLambdaForm    :: LambdaForm    -> out
    , pprUpdateFlag    :: UpdateFlag    -> out
    , pprRec           :: Rec           -> out
    , pprExpr          :: Expr          -> out
    , pprAlts          :: Alts          -> out
    , pprAlgebraicAlts :: AlgebraicAlts -> out
    , pprPrimitiveAlts :: PrimitiveAlts -> out
    , pprAlgebraicAlt  :: AlgebraicAlt  -> out
    , pprPrimitiveAlt  :: PrimitiveAlt  -> out
    , pprDefaultAlt    :: DefaultAlt    -> out
    , pprLiteral       :: Literal       -> out
    , pprPrimOp        :: PrimOp        -> out
    , pprVar           :: Var           -> out
    , pprVars          :: [Var]         -> out
    , pprAtom          :: Atom          -> out
    , pprAtoms         :: [Atom]        -> out
    , pprConstr        :: Constr        -> out
    }
