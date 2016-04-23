{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}

{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- | Remove unused heap objects.
module Stg.Machine.GarbageCollection (
    garbageCollect,
    Dead(..),
    Alive(..),
) where



import           Data.Map          (Map)
import qualified Data.Map          as M
import           Data.Monoid
import           Data.Set          (Set)
import qualified Data.Set          as S

import           Stack
import           Stg.Language
import           Stg.Machine.Types



-- | Alive objects.
newtype Alive a = Alive a
    deriving (Eq, Ord, Show, Monoid)

-- | Dead objects that been eliminated by garbage collection.
newtype Dead a = Dead a
    deriving (Eq, Ord, Show, Monoid)

garbageCollect :: StgState -> (Dead Heap, Alive Heap)
garbageCollect StgState
    { stgCode        = code
    , stgHeap        = heap
    , stgGlobals     = globals
    , stgArgStack    = argS
    , stgReturnStack = retS
    , stgUpdateStack = updS }
  = let GcState {aliveHeap = alive, oldHeap = dead}
            = until everythingCollected splitHeap start

        start = GcState
            { aliveHeap     = mempty
            , oldHeap       = heap
            , evacuate = (Alive . mconcat)
                [ addrs code, addrs globals
                , addrs argS, addrs retS, addrs updS ] }
    in (Dead dead, alive)

everythingCollected :: GcState -> Bool
everythingCollected GcState{evacuate = Alive x} = S.null x

data GcState = GcState
    { aliveHeap :: Alive Heap
        -- ^ Heap of closures known to be alive.
        --   Has no overlap with the old heap.

    , evacuate  :: Alive (Set MemAddr)
        -- ^ Memory addresses known to be alive,
        --   but not yet scavenged from the old heap.

    , oldHeap   :: Heap
        -- ^ The old heap, containing both dead
        --   and not-yet-found alive closures.
    } deriving (Eq, Ord, Show)

splitHeap :: GcState -> GcState
splitHeap GcState
    { aliveHeap = oldAlive@(Alive (Heap alive))
    , evacuate  = Alive (evacuateAddrs)
    , oldHeap   = Heap oldRest }
  = GcState
    { aliveHeap = oldAlive <> Alive (Heap scavenged)
    , evacuate  = newParentRescued
    , oldHeap   = Heap newRest }
  where
    -- A closure is alive iff it is on the alive heap, or the closure that
    -- contained it was scavenged in a previous step.
    isAlive addr _closure = M.member addr alive || S.member addr evacuateAddrs

    -- :: (Map MemAddr Closure, Map MemAddr Closure)
    (scavenged, newRest) = M.partitionWithKey isAlive oldRest

    newParentRescued = Alive (addrs scavenged)



-- | Collect all mentioned addresses in a syntax element.
class Addresses a where
    addrs :: a -> Set MemAddr

instance Addresses a => Addresses [a] where
    addrs = foldMap addrs

instance Addresses a => Addresses (Map k a) where
    addrs = foldMap addrs

instance Addresses a => Addresses (Stack a) where
    addrs = foldMap addrs

instance Addresses Code where
    addrs (Eval expr locals)    = addrs expr <> addrs locals
    addrs (Enter addr)          = [addr]
    addrs (ReturnCon _con args) = addrs args
    addrs (ReturnInt _int)      = []

instance Addresses ArgumentFrame where
    addrs (ArgumentFrame vals) = addrs vals

instance Addresses ReturnFrame where
    addrs (ReturnFrame alts locals) = addrs alts <> addrs locals

instance Addresses UpdateFrame where
    addrs (UpdateFrame argS retS addr) = addrs argS <> addrs retS <> addrs addr

instance Addresses MemAddr where
    addrs addr = [addr]

instance Addresses Globals where
    addrs (Globals globals)
        = S.fromList [ addr | (_, Addr addr) <- M.toList globals ]

instance Addresses Locals where
    addrs (Locals locals) = addrs (Globals locals)

instance Addresses Closure where
    addrs (Closure lf free) = addrs lf <> addrs free

instance Addresses LambdaForm where
    addrs (LambdaForm _free _upd _bound expr) = addrs expr

instance Addresses Value where
    addrs = \case
        Addr s     -> S.singleton s
        PrimInt _i -> mempty

instance Addresses Expr where
    addrs = \case
        Let _rec binds expr -> addrs binds <> addrs expr
        Case scrutinee alts -> addrs scrutinee <> addrs alts
        AppF _fun args      -> addrs args
        AppC _con args      -> addrs args
        AppP _f _x _y       -> mempty
        Lit _i              -> mempty

instance Addresses Binds where
    addrs (Binds bs) = foldMap addrs bs

instance Addresses Alts where
    addrs = \case
        Algebraic alts -> addrs alts
        Primitive alts -> addrs alts

instance Addresses AlgebraicAlts where
    addrs (AlgebraicAlts alts defaultAlt) = addrs alts <> addrs defaultAlt

instance Addresses PrimitiveAlts where
    addrs (PrimitiveAlts alts defaultAlt) = addrs alts <> addrs defaultAlt

instance Addresses AlgebraicAlt where
    addrs (AlgebraicAlt _con _vars expr) = addrs expr

instance Addresses PrimitiveAlt where
    addrs (PrimitiveAlt _prim expr) = addrs expr

instance Addresses DefaultAlt where
    addrs = \case
        DefaultNotBound expr -> addrs expr
        DefaultBound _  expr -> addrs expr

instance Addresses Atom where
    addrs = mempty
