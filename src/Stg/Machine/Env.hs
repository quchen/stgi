{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Defines operations on local and global variable environments.
module Stg.Machine.Env (

    -- * Locals
    addLocals,
    makeLocals,
    val,
    vals,
    localVal,
    NotInScope(..),

    -- * Globals
    makeGlobals,
    globalVal,
) where



import           Control.Applicative
import qualified Data.Map            as M
import           Data.Monoid

import           Stg.Language
import           Stg.Machine.Types
import           Stg.Util



-- | Type safety wrapper.
newtype NotInScope = NotInScope [Var]
    deriving (Eq, Ord, Show, Monoid)



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
val :: Locals -> Globals -> Atom -> Validate NotInScope Value
val _lcl _gbl (AtomLit (Literal k)) = Success (PrimInt k)
val (Locals locals) (Globals globals) (AtomVar var) =
    case M.lookup var locals <|> M.lookup var globals of
        Just v -> Success v
        Nothing -> Failure (NotInScope [var])

-- | Look up the values of many 'Atom's, and return their values in the
-- input's order, or a list of variables not in scope.
vals :: Locals -> Globals -> [Atom] -> Validate NotInScope [Value]
vals locals globals = traverse (val locals globals)

-- | Look up the value of a variable in the local environment.
localVal :: Locals -> Var -> Validate NotInScope Value
localVal locals = val locals mempty . AtomVar

-- | Create the global environment from a list of bindings.
makeGlobals :: [(Var, Value)] -> Globals
makeGlobals = Globals . M.fromList

-- | Look up the value of a variable in the global environment.
globalVal :: Globals -> Var -> Validate NotInScope Value
globalVal globals = val mempty globals . AtomVar
