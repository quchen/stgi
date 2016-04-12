-- | Defines a dictionary to build prettyprinters with.
module Stg.Language.Prettyprint.Module (

    -- * Dictionaries
    makePrettyprinter,
    PrettyprinterDict(..),

    -- * Modules
    PrettyprinterModule,
    makeModule,
    modifyModule,

) where



import           Data.Function
import           Prelude       hiding ((<$>))

import           Stg.Language



-- | Create a dictionary from a module.
--
-- Internally, this resolves circular references in the module, tying the
-- knot to yield a single dictionary of potentially mutually recursive entries.
makePrettyprinter :: PrettyprinterModule out -> PrettyprinterDict out
makePrettyprinter (PrettyprinterModule makeDict) = fix makeDict



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



-- | A module is a potentially mutually recursive collection of
-- functions to prettyprint to an output of type @out@.
newtype PrettyprinterModule out =
    PrettyprinterModule (PrettyprinterDict out -> PrettyprinterDict out)



-- | Create a module out of a modifier for dictionaries. Useful for initial
-- creation of a module; to modify an existing one, use 'modifyModule'.
--
-- For a practical use of this, see 'parserInverseModule'.
makeModule
    :: (PrettyprinterDict out -> PrettyprinterDict out)
    -> PrettyprinterModule out
makeModule = PrettyprinterModule



-- | Modify a 'PrettyprinterModule' by mapping its contained
-- 'PrettyprinterDict' to an altered version.
--
-- For example, if you want to modify a prettyprinter to omit the
-- update flag in output, use
--
-- @
-- 'modifyModule' pprModule (\\dict -> dict
--     { 'pprUpdateFlag' = \\_dict _upd -> 'empty' } )
-- @
--
-- For a practical use of this, see 'parserInverseAnsiModule'.
--
-- To create a new module, use 'makeModule'.
modifyModule
    :: PrettyprinterModule out
    -> (PrettyprinterDict out -> PrettyprinterDict out)
    -> PrettyprinterModule out
modifyModule (PrettyprinterModule makeDict) modifier
    = PrettyprinterModule (modifier . makeDict)
