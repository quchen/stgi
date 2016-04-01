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


addLocals :: [(Var, Value)] -> Locals -> Locals
addLocals defs locals = makeLocals defs <> locals

makeLocals :: [(Var, Value)] -> Locals
makeLocals = Locals . M.fromList

val :: Locals -> Globals -> Atom -> Maybe Value
val (Locals locals) (Globals globals) = \case
    AtomLit (Literal k) -> Just (PrimInt k)
    AtomVar v           -> M.lookup v locals <|> M.lookup v globals

vals :: Locals -> Globals -> [Atom] -> Maybe [Value]
vals locals globals = traverse (val locals globals)

unsafeVals :: Locals -> Globals -> [Atom] -> [Value]
unsafeVals l g a = fromMaybe (error "Variable not found") (vals l g a)

localVal :: Locals -> Var -> Maybe Value
localVal (Locals locals) var = M.lookup var locals

makeGlobals :: [(Var, Value)] -> Globals
makeGlobals = Globals . M.fromList

globalVal :: Globals -> Var -> Maybe Value
globalVal (Globals globals) var = M.lookup var globals
