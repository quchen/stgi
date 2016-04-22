-- | The STG heap maps memory addresses to closures.
module Stg.Machine.Heap (
    size,
    lookup,
    update,
    alloc,
    allocMany,

    -- * Garbage collection
    garbageCollect,
) where



import qualified Data.Map                           as M
import           Prelude                            hiding (lookup)

import           Stg.Machine.Heap.GarbageCollection
import           Stg.Machine.Types



-- | Current number of elements in a heap.
size :: Heap -> Int
size (Heap heap) = M.size heap

-- | Look up a value on the heap.
lookup :: MemAddr -> Heap -> Maybe Closure
lookup addr (Heap heap) = M.lookup addr heap

-- | Update a value on the heap.
update :: MemAddr -> Closure -> Heap -> Heap
update addr cl (Heap h) = Heap (M.adjust (const cl) addr h)

-- | Store a value in the heap at an unused address.
alloc :: Closure -> Heap -> (MemAddr, Heap)
alloc lambdaForm (Heap h) = (addr, heap')
  where
    addr = MemAddr (case M.maxViewWithKey h of
        Just ((MemAddr x,_),_) -> x+1
        Nothing                -> 0 )
    heap' = Heap (M.insert addr lambdaForm h)

-- | Store many values in the heap at unused addresses, and return them
-- in input order.
allocMany :: [Closure] -> Heap -> ([MemAddr], Heap)
allocMany [] heap = ([], heap)
allocMany (cl:cls) heap =
    let (addr, heap') = alloc cl heap
        (addrs, heap'') = allocMany cls heap'
    in (addr:addrs, heap'')
