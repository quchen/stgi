{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

-- | Two space stop-and-copy algorithm.
module Stg.Machine.GarbageCollection.TwoSpaceSopAndCopy (
    garbageCollect,

    -- * Low-level
    splitHeap,
    Dead(..),
    Alive(..),
) where



import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import           Data.Traversable

import qualified Stg.Machine.Heap  as H
import           Stg.Machine.Types



garbageCollect :: StgState -> StgState
garbageCollect stgState
  = let (Dead deadHeap, Alive cleanHeap) = splitHeap stgState
    in if H.size deadHeap > 0
        then stgState { stgHeap  = cleanHeap
                   , stgTicks = stgTicks stgState + 1
                   , stgInfo  = Info GarbageCollection
                                     [Detail_GarbageCollected (toList (H.addresses deadHeap))] }
        else stgState

-- | Alive objects.
newtype Alive a = Alive a
    deriving (Eq, Ord, Show, Monoid)

-- | Dead objects that been eliminated by garbage collection.
newtype Dead a = Dead a
    deriving (Eq, Ord, Show, Monoid)

-- | Old objects represent the previous generation of alive objects, and
-- contain both dead and alive objects.
newtype Old a = Old a
    deriving (Eq, Ord, Show, Monoid)

-- | Split the heap of an 'StgState' in two components: dead (can be discarded)
-- and alive (are still used) closures.
splitHeap :: StgState -> (Dead Heap, Alive Heap)
splitHeap StgState
    { stgCode    = code
    , stgHeap    = heap
    , stgGlobals = globals
    , stgStack   = stack }
  = let GcState {aliveHeap = alive, oldHeap = old}
            = execState twoSpaceSopAndCopy GcState
                { aliveHeap = mempty
                , forwards = mempty
                , toEvacuate = (Alive . mconcat)
                    [addrs code, addrs globals, addrs stack]
                , oldHeap = Old heap }
        dead = let Alive (Heap a) = alive
                   Old (Heap o)   = old
               in Dead (Heap (o `M.difference` a))
    in (dead, alive)

twoSpaceSopAndCopy :: State GcState ()
twoSpaceSopAndCopy = do
    GcState { toEvacuate = Alive evacuateNext
            , oldHeap = Old old } <- get
    unless (S.null evacuateNext) (do
        toAddrs <- for (toList evacuateNext) evacuate
        for_ toAddrs (\toAddr -> case H.lookup toAddr old of
            Nothing -> error "twoSpaceSopAndCopy error: address not found!"
            Just heapObject -> scavenge heapObject )
        twoSpaceSopAndCopy )

data GcState = GcState
    { aliveHeap :: Alive Heap
        -- ^ Heap of closures known to be alive.
        --   Has no overlap with the old heap.

    , forwards :: Map MemAddr MemAddr

    , toEvacuate :: Alive (Set MemAddr)
        -- ^ Closures known to be alive, but not yet evacuated

    , oldHeap :: Old Heap
    } deriving (Eq, Ord, Show)

followForwardChain :: MemAddr -> Map MemAddr MemAddr -> MemAddr
followForwardChain addr forw = case M.lookup addr forw of
    Just addr' -> followForwardChain addr' forw
    Nothing    -> addr

-- | Copy a closure from from-space to to-space, and return the new memory
-- address.
evacuate :: MemAddr -> State GcState MemAddr
evacuate addr = do
    gcState@GcState
        { aliveHeap = Alive alive
        , forwards  = forw
        , oldHeap   = Old old } <- get

    -- Check whether the address has previously been collected
    case M.lookup addr forw of

        Nothing -> case H.lookup (followForwardChain addr forw) old of
            Nothing -> error "Tried collecting a non-existent memory address!"
            Just heapObject -> do
                let (addr', alive') = H.alloc heapObject alive
                    forwards' = M.insert addr addr' forw
                put gcState { aliveHeap = Alive alive'
                            , forwards = forwards' }
                pure addr'

        -- Forward pointer exists, so the object has previously been collected.
        -- Don't do anything to it.
        Just forward -> pure forward

scavenge :: HeapObject -> State GcState HeapObject
scavenge bh@Blackhole{} = pure bh
scavenge (HClosure (Closure lf frees)) = do
    frees' <- for frees (\case
        Addr addr -> fmap Addr (evacuate addr )
        PrimInt i -> pure (PrimInt i) )
    pure (HClosure (Closure lf frees'))



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
