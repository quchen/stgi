{-# LANGUAGE LambdaCase #-}

module Stg.Machine.Env (

    -- * Locals
    addLocals,
    makeLocals,
    val,
    vals,
    unsafeVals,
    localVal,

    -- * Globals
    makeGlobals,
    globalVal,
) where



import           Control.Applicative
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Monoid

import           Stg.Language
import           Stg.Machine.Types


-- | Add a list of bindings to the local environment.
--
-- Already existing variables will be shadowed (i.e. overwritten).
addLocals :: [(Var, Value)] -> Locals -> Locals
addLocals defs locals = makeLocals defs <> locals

-- | Create a local environment from a list of bindings.
makeLocals :: [(Var, Value)] -> Locals
makeLocals = Locals . M.fromList

-- | Look up the value of an 'Atom' first in the local, then in the global
-- environment.
val :: Locals -> Globals -> Atom -> Maybe Value
val (Locals locals) (Globals globals) = \case
    AtomLit (Literal k) -> Just (PrimInt k)
    AtomVar v           -> M.lookup v locals <|> M.lookup v globals

-- | Look up the values of many 'Atom's, and return their values in the
-- input's order.
vals :: Locals -> Globals -> [Atom] -> Maybe [Value]
vals locals globals = traverse (val locals globals)

-- | 'vals' that 'error's if the 'Atom' is not in the environment.
unsafeVals :: Locals -> Globals -> [Atom] -> [Value]
unsafeVals l g a = fromMaybe (error "Variable not found") (vals l g a)

-- | Look up the value of a variable in the local environment.
localVal :: Locals -> Var -> Maybe Value
localVal (Locals locals) var = M.lookup var locals

-- | Create the global environment from a list of bindings.
makeGlobals :: [(Var, Value)] -> Globals
makeGlobals = Globals . M.fromList

-- | Look up the value of a variable in the global environment.
globalVal :: Globals -> Var -> Maybe Value
globalVal (Globals globals) var = M.lookup var globals
