{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}

{-# OPTIONS_GHC -fdefer-typed-holes #-}

-- | Remove unused heap objects.
module Stg.Machine.Heap.GarbageCollection (
    garbageCollect,
    Dead(..),
    Alive(..),
) where



import           Data.Map          (Map)
import qualified Data.Map          as M
import           Data.Monoid
import           Data.Set          (Set)
import qualified Data.Set          as S

import           Stg.Language
import           Stg.Machine.Types



-- | Alive objects.
newtype Alive a = Alive a
    deriving (Eq, Ord, Show, Monoid)

-- | Dead objects that been eliminated by garbage collection.
newtype Dead a = Dead a
    deriving (Eq, Ord, Show, Monoid)

garbageCollect :: Globals -> Heap -> (Dead Heap, Alive Heap)
garbageCollect globals heap = (Dead dead, alive)
  where
    GcState {aliveHeap = alive, oldHeap = dead}
        = until everythingCollected splitHeap start

    start = GcState
        { aliveHeap     = mempty
        , parentRescued = Alive (globalAddrs globals)
        , oldHeap       = heap
        }

everythingCollected :: GcState -> Bool
everythingCollected GcState{parentRescued = Alive x} = S.null x

data GcState = GcState
    { aliveHeap     :: Alive Heap
        -- ^ Heap of closures known to be alive.
        --   Has no overlap with the old heap.

    , parentRescued :: Alive (Set MemAddr)
        -- ^ Memory addresses known to be alive,
        --   but not yet scavenged from the old heap.

    , oldHeap       :: Heap
        -- ^ The old heap, containing both dead
        --   and not-yet-found alive closures.
    } deriving (Eq, Ord, Show)

-- | Find all addresses of global values.
globalAddrs :: Globals -> Set MemAddr
globalAddrs (Globals globals)
  = S.fromList [ addr | (_, Addr addr) <- M.toList globals ]

splitHeap :: GcState -> GcState
splitHeap GcState
    { aliveHeap     = oldAlive@(Alive (Heap alive))
    , parentRescued = Alive (parentRescuedAddrs)
    , oldHeap       = Heap oldRest }
  = GcState
    { aliveHeap     = oldAlive <> Alive (Heap scavenged)
    , parentRescued = newParentRescued
    , oldHeap       = Heap newRest }
  where
    -- A closure is alive iff it is on the alive heap, or the closure that
    -- contained it was scavenged in a previous step.
    isAlive addr _closure = M.member addr alive || S.member addr parentRescuedAddrs

    -- :: (Map MemAddr Closure, Map MemAddr Closure)
    (scavenged, newRest) = M.partitionWithKey isAlive oldRest

    newParentRescued = Alive (addrs scavenged)



-- | Collect all mentioned addresses in a syntax element.
class Addresses ast where
    addrs :: ast -> Set MemAddr

instance Addresses ast => Addresses [ast] where
    addrs = foldMap addrs

instance Addresses ast => Addresses (Map k ast) where
    addrs = foldMap addrs

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
