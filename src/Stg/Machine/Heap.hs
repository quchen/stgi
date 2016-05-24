-- | The STG heap maps memory addresses to closures.
module Stg.Machine.Heap (
    -- * Info
    size,
    addresses,

    -- * Management
    lookup,
    update,
    updateMany,
    alloc,
    allocMany,
) where



import qualified Data.List   as L
import qualified Data.Map    as M
import           Data.Monoid
import           Data.Set    (Set)
import           Prelude     hiding (lookup)

import Stg.Machine.Types



-- | Current number of elements in a heap.
size :: Heap -> Int
size (Heap heap) = M.size heap

-- | Look up a value on the heap.
lookup :: MemAddr -> Heap -> Maybe HeapObject
lookup addr (Heap heap) = M.lookup addr heap

-- | Update a value on the heap.
update :: MemAddr -> HeapObject -> Heap -> Heap
update addr obj (Heap h) = Heap (M.adjust (const obj) addr h)

-- | Update many values on the heap.
updateMany :: [MemAddr] -> [HeapObject] -> Heap -> Heap
updateMany addrs objs heap =
    L.foldl' (\h (addr, obj) -> update addr obj h) heap (zip addrs objs)

-- | Store a value in the heap at an unused address.
alloc :: HeapObject -> Heap -> (MemAddr, Heap)
alloc lambdaForm heap = (addr, heap')
  where
    ([addr], heap') = allocMany [lambdaForm] heap

-- | Store many values in the heap at unused addresses, and return them
-- in input order.
allocMany :: [HeapObject] -> Heap -> ([MemAddr], Heap)
allocMany heapObjects (Heap heap) = (addrs, heap')
  where
    smallestUnusedAddr = case M.maxViewWithKey heap of
        Nothing -> 0
        Just ((MemAddr maxAddr, _), _) -> maxAddr+1
    addrs = zipWith (\addr _obj -> MemAddr addr)
                    [smallestUnusedAddr ..]
                    heapObjects
    heap' = Heap (heap <> M.fromList (zip addrs heapObjects))

-- | All addresses allocated on the heap.
addresses :: Heap -> Set MemAddr
addresses (Heap h) = M.keysSet h
