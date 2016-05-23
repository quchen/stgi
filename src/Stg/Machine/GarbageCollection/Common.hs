{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

-- | Definitions used by various garbage collection algorithms.
module Stg.Machine.GarbageCollection.Common (
    splitHeapWith,
    GarbageCollectionAlgorithm(..),
    Dead(..),
    Alive(..),
    Addresses(..),
) where



import           Data.Set (Set)
import qualified Data.Set as S

import Stg.Machine.Types



-- | Split the heap contained in a machine state in two parts: the dead objects
-- that can safely be discarded, and the alive ones that are still needed by
-- the program.
splitHeapWith
    :: GarbageCollectionAlgorithm
    -> StgState
    -> (Dead Heap, Alive Heap)
splitHeapWith (GarbageCollectionAlgorithm gc) = gc

newtype GarbageCollectionAlgorithm
  = GarbageCollectionAlgorithm (StgState -> (Dead Heap, Alive Heap))

-- | Alive objects.
newtype Alive a = Alive a
    deriving (Eq, Ord, Show, Monoid)

-- | Dead objects that been eliminated by garbage collection.
newtype Dead a = Dead a
    deriving (Eq, Ord, Show, Monoid)



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
