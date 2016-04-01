{-# LANGUAGE LambdaCase #-}

module Stg.Machine.Env (
    addLocal,
    addLocals,
    makeLocals,
    val,
    vals,
    unsafeVals,
    localVal,
) where



import           Control.Applicative
import qualified Data.Map            as M
import           Data.Maybe

import           Stg.Language
import           Stg.Machine.Types



addLocal :: (Var, Value) -> Locals -> Locals
addLocal (var, addr) (Locals locals) = Locals (M.insert var addr locals)

addLocals :: [(Var, Value)] -> Locals -> Locals
addLocals defs locals = foldr addLocal locals defs

makeLocals :: [(Var, Value)] -> Locals
makeLocals defs = addLocals defs mempty

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
