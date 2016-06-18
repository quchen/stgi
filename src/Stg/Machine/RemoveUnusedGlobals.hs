{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | "Garbage collection for globals". This allows importing the entire Prelude
-- without polluting the program with unnecessary definitions, for example.
module Stg.Machine.RemoveUnusedGlobals (
    removeUnusedGlobals,
) where



import qualified Data.Map    as M
import           Data.Monoid hiding (Alt)
import           Data.Set    (Set)
import qualified Data.Set    as S
import qualified Data.Text   as T

import Stg.Language
import Stg.Language.Prettyprint
import Stg.Machine.Types



-- | Remove globals that are not used in the program.
--
-- After globals have been removed, running the garbage collector will be able
-- to clean off the leftovers.
removeUnusedGlobals
    :: StgState
    -> StgState
removeUnusedGlobals state@StgState { stgGlobals = Globals globals }
  = let setToMap = M.fromSet (const ())
        usedVars = usagesInState state
        globals' = globals `M.intersection` setToMap usedVars
        _removedGlobals = globals `M.difference` globals'
    in state
        { stgGlobals = Globals globals'
        , stgInfo = Info (StateError RemoveUnusedGlobals) []
        }

data UsageSet = UsageSet (Set Var) (Set MemAddr)

instance Monoid UsageSet where
    mempty = UsageSet mempty mempty
    UsageSet a b `mappend` UsageSet x y = UsageSet (a <> x) (b <> y)

withoutVarsFrom :: UsageSet -> UsageSet -> UsageSet
withoutVarsFrom (UsageSet vars1 addrs1) (UsageSet vars2 _addrs2)
  = UsageSet (vars1 `S.difference` vars2) addrs1

-- | Collect all reachable global variables.
class Usages a where
    usages :: a -> UsageSet

instance (Foldable f, Usages a) => Usages (f a) where
    usages = foldMap usages

usagesInState :: StgState -> Set Var
usagesInState StgState
    { stgCode        = code
    , stgHeap        = heap
    , stgStack       = stack
    , stgGlobals     = globals }
  = error "implement usagesInState" code heap stack globals

instance Usages Code where
    usages (Eval expr locals)    = usages expr <> usages locals
    usages (Enter addr)          = usages addr
    usages (ReturnCon _con args) = usages args
    usages (ReturnInt _)         = mempty

instance Usages StackFrame where
    usages = \case
        ArgumentFrame arg       -> usages arg
        ReturnFrame alts locals -> usages alts <> usages locals
        UpdateFrame addr        -> usages addr

instance Usages Locals where
    usages (Locals locals) = usages (M.elems locals)

instance Usages Closure where
    usages (Closure lf freeValues) = usages lf <> usages freeValues

instance Usages LambdaForm where
    usages (LambdaForm free _upd bound expr)
      = usages expr `withoutVarsFrom` (usages free <> usages bound)

instance Usages Expr where
    usages = \case
        Let _rec binds expr -> usages binds <> usages expr
        Case scrutinee alts -> usages scrutinee <> usages alts
        AppF fun args       -> usages fun <> usages args
        AppC _con args      -> usages args
        AppP _f _x _y       -> mempty
        Lit _i              -> mempty

instance Usages Binds where
    usages (Binds bs) = foldMap usages bs

instance Usages Alts where
    usages (Alts nondef def) = usages nondef <> usages def

instance Usages NonDefaultAlts where
    usages = \case
        NoNonDefaultAlts   -> mempty
        AlgebraicAlts alts -> usages alts
        PrimitiveAlts alts -> usages alts

instance Usages AlgebraicAlt where
    usages = \case AlgebraicAlt _con _args expr -> usages expr

instance Usages PrimitiveAlt where
    usages = \case PrimitiveAlt _lit expr -> usages expr

instance Usages DefaultAlt where
    usages = \case
        DefaultNotBound expr     -> usages expr
        DefaultBound patVar expr -> usages expr `withoutVarsFrom` usages patVar

instance Usages Atom where
    usages (AtomVar v) = usages v
    usages (AtomLit _) = mempty

instance Usages Value where
    usages (Addr val)  = usages val
    usages (PrimInt _) = mempty

instance Usages Var where
    usages v = UsageSet [v] mempty

instance Usages MemAddr where
    usages addr = UsageSet mempty [addr]
