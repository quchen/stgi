{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}

-- | Remove unused heap objects.
module Stg.Machine.Heap.GarbageCollection (
    garbageCollect,
    Dead(..),
    Alive(..),
) where



import           Data.Map          (Map)
import qualified Data.Map          as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set          (Set)
import qualified Data.Set          as S

import           Stg.Language
import           Stg.Machine.Types



-- | Alive memory addresses.
newtype Alive = Alive (Set MemAddr)

-- | Dead memory addresses that have been eliminated by garbage collection.
newtype Dead = Dead (Set MemAddr)

-- | Simple tracing garbage collector.
--
-- 1. Get the addresses of all globals.
-- 2. Collect all the addresses contained in the closures the global addresses
--    map to on the heap.
-- 3. Drop all addresses from the heap that weren't found in the process.
garbageCollect
    :: Globals -- ^ Root elements (unconditionally alive).
    -> Heap
    -> (Dead, Alive, Heap)
garbageCollect globals h@(Heap heap) = (Dead dead, Alive alive, Heap cleanHeap)
  where
    alive = aliveAddresses globals h
    dead = M.keysSet heap `S.difference` alive
    (cleanHeap, _deadHeap) = M.partitionWithKey isAlive heap
    isAlive addr _ = S.member addr alive

-- garbageCollect'
--     :: Globals -- ^ Root elements (unconditionally alive).
--     -> Heap
--     -> (Dead, Alive, Heap)
garbageCollect' globals heap = loop ([], alive, alive, heap)
  where
    alive = aliveAddresses globals heap
    loop x@(_, _, [], _) = x
    loop x = loop (gcStep x)
    gcStep (dead, alive, toConsider, heap) = (dead', alive', toConsider', heap')
      where
        (aliveHeap, deadHeap) = let isAlive addr _ = S.member addr alive
                                    Heap h = heap
                                in M.partitionWithKey isAlive h
        dead'  = dead  <> M.keysSet deadHeap
        alive' = alive <> M.keysSet aliveHeap
        toConsider' = M.keysSet aliveHeap
        heap' = Heap aliveHeap

garbageCollect2 globals heap = evacLoop ([], globalHeap, heap)
  where
    globalHeap = _aliveAddresses2 (M.intersection (let Globals gbl = globals in gbl)
                                                  (let Heap hp = heap in hp))
                                                  heap

    evacLoop :: (Heap, Heap, Heap) -> (Heap, Heap)
    evacLoop (dead, alive, toConsider) | let Heap foo = toConsider in M.null foo = (dead, alive)
    evacLoop (dead, alive, toConsider) =
        let (dead', alive') = M.partition (\addr -> M.member addr alive) toConsider
        in _

    -- | Find all alive addresses in the heap, starting at the values of the
    -- globals, which are considered alive.
    aliveAddresses2 :: Set MemAddr -> Heap -> Set MemAddr
    aliveAddresses2 alive (Heap heap) = foldMap addrs aliveClosures
      where
        aliveAddrs = [ addr | Addr addr <- S.toList alive ] -- TODO: don't use list, Set is sufficient
        aliveClosures = mapMaybe (\addr -> M.lookup addr heap) aliveAddrs

-- | Find all alive addresses in the heap, starting at the values of the
-- globals, which are considered alive.
aliveAddresses :: Globals -> Heap -> Set MemAddr
aliveAddresses (Globals globals) (Heap heap) = foldMap addrs globalClosures
  where
    globalAddrs = [ addr | (_, Addr addr) <- M.toList globals ]
    globalClosures = mapMaybe (\addr -> M.lookup addr heap) globalAddrs



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
