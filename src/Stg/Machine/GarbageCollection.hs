{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

-- | Remove unused heap objects.
module Stg.Machine.GarbageCollection (
    garbageCollect,

    -- * Low-level
    splitHeap,
    Dead(..),
    Alive(..),
) where



import           Data.Foldable
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Monoid   hiding (Alt)
import           Data.Set      (Set)
import qualified Data.Set      as S

import qualified Stg.Machine.Heap  as H
import           Stg.Machine.Types



garbageCollect :: StgState -> StgState
garbageCollect state
  = let (Dead deadHeap, Alive cleanHeap) = splitHeap state
    in if H.size deadHeap > 0
        then state { stgHeap  = cleanHeap
                   , stgTicks = stgTicks state + 1
                   , stgInfo  = Info GarbageCollection
                                     [Detail_GarbageCollected (toList (H.addresses deadHeap))] }
        else state

-- | Alive objects.
newtype Alive a = Alive a
    deriving (Eq, Ord, Show, Monoid)

-- | Dead objects that been eliminated by garbage collection.
newtype Dead a = Dead a
    deriving (Eq, Ord, Show, Monoid)

-- | Split the heap of an 'StgState' in two components: dead (can be discarded)
-- and alive (are still used) closures.
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
everythingCollected = noAlives
  where
    noAlives GcState {evacuate = Alive alive} = S.null alive

data GcState = GcState
    { aliveHeap :: Alive Heap
        -- ^ Heap of closures known to be alive.
        --   Has no overlap with the old heap.

    , evacuate :: Alive (Set MemAddr)
        -- ^ Memory addresses known to be alive,
        --   but not yet scavenged from the old heap.

    , oldHeap :: Heap
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
    , evacuate  = Alive (addrs scavenged)
    , oldHeap   = Heap newRest }
  where
    scavenged, newRest :: Map MemAddr HeapObject
    (scavenged, newRest) = M.partitionWithKey isAlive oldRest
      where
        -- A closure is alive iff it is on the alive heap, or the closure that
        -- contained it was scavenged in a previous step.
        isAlive addr _closure = M.member addr alive
                             || S.member addr evacuateAddrs



-- | Collect all mentioned addresses in a machine element.
--
-- Note that none of the types in "Stg.Language" contain addresses, since an
-- address is not something present in the STG _language_, only in the execution
-- contest the language is put in in the "Stg.Machine" modules.
class Addresses a where
    addrs :: a -> Set MemAddr

instance (Foldable f, Addresses a) => Addresses (f a) where
    addrs = foldMap addrs

instance Addresses Code where
    addrs = \case
        Eval _expr locals   -> addrs locals
        Enter addr          -> addrs addr
        ReturnCon _con args -> addrs args
        ReturnInt _int      -> mempty

instance Addresses StackFrame where
    addrs = \case
        ArgumentFrame vals       -> addrs vals
        ReturnFrame _alts locals -> addrs locals
        UpdateFrame addr         -> addrs addr

instance Addresses MemAddr where
    addrs addr = S.singleton addr

instance Addresses Globals where
    addrs (Globals globals) = addrs globals

instance Addresses Locals where
    addrs (Locals locals) = addrs locals

instance Addresses Closure where
    addrs (Closure _lf free) = addrs free

instance Addresses HeapObject where
    addrs = \case
        HClosure closure  -> addrs closure
        Blackhole _bhTick -> mempty

instance Addresses Value where
    addrs = \case
        Addr addr  -> addrs addr
        PrimInt _i -> mempty
