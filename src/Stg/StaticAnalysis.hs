{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

-- | Extract Haskell values from running STG programs.
module Stg.StaticAnalysis (
    FreeVariables (..),
    FVars (..),
) where



import           Data.Map    as M
import           Data.Monoid
import           Data.Set    (Set)
import qualified Data.Set    as S

import Stg.Language



(-<>) :: FVars -> FVars -> FVars
FVars a b -<> FVars x y= FVars (a `S.difference` x) (b `S.difference` y)
infix 6 -<> -- 6 like <>

data FVars = FVars
    { allFree :: Set Var
        -- ^ All free variables that were found

    , explicitlyFree :: Set Var
        -- ^ Only the free variables that are in the explicit free variable
        -- list of a lambda form
    } deriving (Eq, Ord, Show)

instance Monoid FVars where
    mempty = FVars mempty mempty
    FVars a b `mappend` FVars x y = FVars (a <> x) (b <> y)

class FreeVariables ast where
    freeVariables :: ast -> FVars

instance (Foldable f, FreeVariables a) => FreeVariables (f a) where
    freeVariables = foldMap freeVariables

instance FreeVariables Program where
    freeVariables (Program prog) = freeVariables prog

instance FreeVariables Binds where
    freeVariables (Binds bs) = freeVariables bs

bindNames :: Binds -> FVars
bindNames (Binds bs) = let names = M.keysSet bs in FVars names names

instance FreeVariables Expr where
    freeVariables = \case
        Let _rec binds expr -> (freeVariables expr -<> bindNames binds)
                                <> freeVariables binds
        Case expr alts      -> freeVariables expr <> freeVariables alts
        AppF f args         -> freeVariables f    <> freeVariables args
        AppC _con args      -> freeVariables args
        AppP _op arg1 arg2  -> freeVariables arg1 <> freeVariables arg2
        Lit lit             -> freeVariables lit

instance FreeVariables LambdaForm where
    -- The free variables of a lambda form are the set of explicitly named
    -- free variables, and all the globals used in it. We ignore the explicit
    -- free variable list here, so that we retain the flexibility of analyzing
    -- which free variables were global later.
    freeVariables (LambdaForm frees _upd bound expr)
      = FVars { allFree = allFree (freeVariables expr -<> freeVariables bound)
              , explicitlyFree = S.fromList frees }

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
    freeVariables var = let x = S.singleton var in FVars x x

instance FreeVariables Literal where
    freeVariables _lit = mempty

instance FreeVariables Atom where
    freeVariables = \case
        AtomVar var -> freeVariables var
        AtomLit lit -> freeVariables lit
