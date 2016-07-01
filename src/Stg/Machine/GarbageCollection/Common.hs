{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

-- | Definitions used by various garbage collection algorithms.
module Stg.Machine.GarbageCollection.Common (
    splitHeapWith,
    GarbageCollectionAlgorithm(..),
    Addresses(addrs),
    UpdateAddrs(..),
) where



import           Data.Map      (Map)
import           Data.Sequence (Seq, ViewL (..), (<|))
import qualified Data.Sequence as Seq
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Data.Text

import Stg.Machine.Types



-- | Split the heap contained in a machine state in three parts: the dead
-- objects that can safely be discarded, a maping from old to new addresses if
-- definitions were moved, and the final state with a cleaned up heap.
splitHeapWith
    :: GarbageCollectionAlgorithm
    -> StgState
    -> (Set MemAddr, Map MemAddr MemAddr, StgState)
splitHeapWith (GarbageCollectionAlgorithm _name gc) = gc

data GarbageCollectionAlgorithm = GarbageCollectionAlgorithm
    Text
    (StgState -> (Set MemAddr, Map MemAddr MemAddr, StgState))
        -- ^ Dead addresses, moved addresses, new state

-- | Collect all mentioned addresses in a machine element.
--
-- Note that none of the types in "Stg.Language" contain addresses, since an
-- address is not something present in the STG __language__, only in the execution
-- contest the language is put in in the "Stg.Machine" modules.
class Addresses a where
    -- | All contained addresses in the order they appear, but without
    -- duplicates.
    addrs :: a -> Seq MemAddr
    addrs = nubSeq . addrs'

    -- | All contained addresses in the order they appear, with duplicates.
    addrs' :: a -> Seq MemAddr

    {-# MINIMAL addrs' #-}

nubSeq :: Ord a => Seq a -> Seq a
nubSeq = go mempty
  where
    go cache entries = case Seq.viewl entries of
        EmptyL -> mempty
        x :< xs
            | S.member x cache -> go cache xs
            | otherwise -> x <| go (S.insert x cache) xs

instance (Foldable f, Addresses a) => Addresses (f a) where
    addrs' = foldMap addrs'

instance Addresses Code where
    addrs' = \case
        Eval _expr locals   -> addrs' locals
        Enter addr          -> addrs' addr
        ReturnCon _con args -> addrs' args
        ReturnInt _int      -> mempty

instance Addresses StackFrame where
    addrs' = \case
        ArgumentFrame vals       -> addrs' vals
        ReturnFrame _alts locals -> addrs' locals
        UpdateFrame addr         -> addrs' addr

instance Addresses MemAddr where
    addrs' addr = Seq.singleton addr

instance Addresses Globals where
    addrs' (Globals globals) = addrs' globals

instance Addresses Locals where
    addrs' (Locals locals) = addrs' locals

instance Addresses Closure where
    addrs' (Closure _lf free) = addrs' free

instance Addresses HeapObject where
    addrs' = \case
        HClosure closure  -> addrs' closure
        Blackhole _bhTick -> mempty

instance Addresses Value where
    addrs' = \case
        Addr addr  -> addrs' addr
        PrimInt _i -> mempty


-- | Update all contained addresses in a certain value. Useful for moving
-- garbage collectors.
class UpdateAddrs a where
    updateAddrs :: (MemAddr -> MemAddr) -> a -> a

instance UpdateAddrs Code where
    updateAddrs upd = \case
        Eval expr locals      -> Eval expr (updateAddrs upd locals)
        Enter addr            -> Enter (updateAddrs upd addr)
        ReturnCon constr args -> ReturnCon constr (updateAddrs upd args)
        r@ReturnInt{}         -> r

instance UpdateAddrs Locals where
    updateAddrs upd (Locals locals) = Locals (updateAddrs upd locals)

instance UpdateAddrs Globals where
    updateAddrs upd (Globals locals) = Globals (updateAddrs upd locals)

instance UpdateAddrs Value where
    updateAddrs upd = \case
        Addr addr   -> Addr (updateAddrs upd addr)
        p@PrimInt{} -> p

instance UpdateAddrs MemAddr where
    updateAddrs = id

instance (Functor f, UpdateAddrs a) => UpdateAddrs (f a) where
    updateAddrs upd = fmap (updateAddrs upd)

instance UpdateAddrs StackFrame where
    updateAddrs upd = \case
        ArgumentFrame arg       -> ArgumentFrame (updateAddrs upd arg)
        ReturnFrame alts locals -> ReturnFrame alts (updateAddrs upd locals)
        UpdateFrame addr        -> UpdateFrame (updateAddrs upd addr)
