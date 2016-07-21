{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

-- | Extract Haskell values from running STG programs.
module Stg.StaticAnalysis (
    FreeVariables (..),
) where



import           Data.Map    as M
import           Data.Monoid
import           Data.Set    (Set)
import qualified Data.Set    as S

import Stg.Language



(-<>) :: Ord a => Set a -> Set a -> Set a
(-<>) = S.difference
infix 6 -<> -- 6 like <>

-- | Overloading class for determining the free variables of an object.
class FreeVariables ast where
    freeVariables :: ast -> Set Var

instance (Foldable f, FreeVariables a) => FreeVariables (f a) where
    freeVariables = foldMap freeVariables

instance FreeVariables Program where
    freeVariables (Program prog) = freeVariables prog

instance FreeVariables Binds where
    freeVariables (Binds bs) = freeVariables bs

bindNames :: Binds -> Set Var
bindNames (Binds bs) = M.keysSet bs

instance FreeVariables Expr where
    freeVariables = \case
        Let _rec binds expr -> (freeVariables expr -<> bindNames binds)
                                <> freeVariables binds
        Case expr alts      -> freeVariables expr <> freeVariables alts
        AppF f args         -> freeVariables f    <> freeVariables args
        AppC _con args      -> freeVariables args
        AppP _op arg1 arg2  -> freeVariables arg1 <> freeVariables arg2
        LitE lit            -> freeVariables lit

-- | Only takes into account the explicit free variable list of the lambda. This
-- means that globals, which are not explicitly free, will not be considered
-- free variables.
instance FreeVariables LambdaForm where
    freeVariables (LambdaForm frees _upd _bound _expr) = S.fromList frees

instance FreeVariables Alts where
    freeVariables (Alts nonDefaultAlt defaultAlt)
      = freeVariables nonDefaultAlt <> freeVariables defaultAlt

instance FreeVariables NonDefaultAlts where
    freeVariables = \case
        NoNonDefaultAlts   -> mempty
        AlgebraicAlts alts -> freeVariables alts
        PrimitiveAlts alts -> freeVariables alts

instance FreeVariables AlgebraicAlt where
    freeVariables (AlgebraicAlt _con patVars expr)
      = freeVariables expr -<> freeVariables patVars

instance FreeVariables PrimitiveAlt where
    freeVariables (PrimitiveAlt lit expr)
      = freeVariables lit <> freeVariables expr

instance FreeVariables DefaultAlt where
    freeVariables = \case
        DefaultNotBound expr  -> freeVariables expr
        DefaultBound var expr -> freeVariables expr -<> freeVariables var

instance FreeVariables Var where
    freeVariables var = S.singleton var

instance FreeVariables Literal where
    freeVariables _lit = mempty

instance FreeVariables Atom where
    freeVariables = \case
        AtomVar var -> freeVariables var
        AtomLit lit -> freeVariables lit
