module Stg.StaticAnalysis where


class FreeVariables ast where
    freeVariables :: ast -> Set Var

instance (Foldable f, FreeVariables a) => FreeVariables (f a) where
    freeVariables = foldMap freeVariables

instance FreeVariables Expr where
    freeVariables = \case
        Let _rec binds expr
            -> freeVariables binds
                <> freeVariables expr `S.difference` freeVariables binds
        Case expr alts -> freeVariables expr <> freeVariables alts
        AppF f args -> freeVariables f <> freeVariables args
        AppF _con args -> freeVariables args
        AppP _primop arg1 arg2 -> freeVariables arg1 <> freeVariables arg2
        Lit lit -> freeVariables lit

instance FreeVariables LambdaForm where
    freeVariables (LambdaForm frees _upd bound expr)
      = freeVariables expr `S.difference` freeVariables frees
                           `S.difference` freeVariables bound

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
      = freeVariables expr `S.difference` freeVariables patVars

instance FreeVariables PrimitiveAlt where
    freeVariables (PrimitiveAlt _con patVars expr)
      = freeVariables expr `S.difference` freeVariables patVars

instance FreeVariables DefaultAlt where
    freeVariables = \case
        DefaultNotBound expr -> freeVariables expr
        DefaultBound var expr -> freeVariables expr `S.difference` freeVariables var

instance FreeVariables Var where
    freeVariables var = S.singleton var

instance FreeVariables Literal where
    freeVariables _lit = mempty

instance FreeVariables Atom where
    freeVariables = \case
        AtomVar var -> freeVariables var
        AtomLit lit -> freeVariables lit

