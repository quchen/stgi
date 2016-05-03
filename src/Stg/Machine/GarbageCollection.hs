{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Remove unused heap objects.
module Stg.Machine.GarbageCollection (
    garbageCollect,

    -- * Low-level
    splitHeap,
    Dead(..),
    Alive(..),
) where



import qualified Data.Map                as M
import           Data.Monoid             hiding (Alt)
import           Data.Set                (Set)
import qualified Data.Set                as S

import           Stg.Language
import qualified Stg.Machine.Heap        as H
import qualified Stg.Machine.InfoDetails as InfoDetail
import           Stg.Machine.Types



garbageCollect :: StgState -> StgState
garbageCollect state
  = let (Dead deadHeap, Alive cleanHeap) = splitHeap state
    in if H.size deadHeap > 0
        then state { stgHeap  = cleanHeap
                   , stgTicks = stgTicks state + 1
                   , stgInfo  = Info GarbageCollection
                                     (InfoDetail.garbageCollected (H.addresses deadHeap)) }
        else state

-- | Alive objects.
newtype Alive a = Alive a
    deriving (Eq, Ord, Show, Monoid)

-- | Dead objects that been eliminated by garbage collection.
newtype Dead a = Dead a
    deriving (Eq, Ord, Show, Monoid)

-- | Split the heap of an 'StgState' into dead (can be discarded) and alive
-- (are still used) components.
splitHeap :: StgState -> (Dead Heap, Alive Heap)
splitHeap StgState
    { stgCode    = code
    , stgHeap    = heap
    , stgGlobals = globals
    , stgStack   = stack }
  = let GcState {aliveHeap = alive, oldHeap = dead}
            = until everythingCollected gcStep start

        start = GcState
            { aliveHeap = mempty
            , oldHeap = heap
            , evacuate = (Alive . mconcat)
                [addrs code, addrs globals, addrs stack] }
    in (Dead dead, alive)

everythingCollected :: GcState -> Bool
everythingCollected GcState{evacuate = Alive alive} = S.null alive

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

gcStep :: GcState -> GcState
gcStep GcState
    { aliveHeap = oldAlive@(Alive (Heap alive))
    , evacuate  = Alive evacuateAddrs
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

instance (Foldable f, Addresses a) => Addresses (f a) where
    addrs = foldMap addrs

instance Addresses Code where
    addrs (Eval expr locals)    = addrs expr <> addrs locals
    addrs (Enter addr)          = [addr]
    addrs (ReturnCon _con args) = addrs args
    addrs (ReturnInt _int)      = []

instance Addresses StackFrame where
    addrs = \case
        ArgumentFrame vals      -> addrs vals
        ReturnFrame alts locals -> addrs alts <> addrs locals
        UpdateFrame addr        -> addrs addr

instance Addresses MemAddr where
    addrs addr = [addr]

instance Addresses Globals where
    addrs (Globals globals)
        = S.fromList [ addr | (_, Addr addr) <- M.assocs globals ]

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
    addrs (Alts alts def) = addrs alts <> addrs def

instance Addresses Alt where
    addrs (AlgebraicAlt _con _vars expr) = addrs expr
    addrs (PrimitiveAlt _prim      expr) = addrs expr

instance Addresses DefaultAlt where
    addrs = \case
        DefaultNotBound expr -> addrs expr
        DefaultBound _  expr -> addrs expr

instance Addresses Atom where
    addrs = mempty
