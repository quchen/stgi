-- | The STG heap maps memory addresses to closures.
module Stg.Machine.Heap (
    -- * Info
    size,
    addresses,

    -- * Management
    lookup,
    update,
    alloc,
    allocMany,

) where



import qualified Data.List         as L
import qualified Data.Map          as M
import           Data.Monoid
import           Data.Set          (Set)
import           Prelude           hiding (lookup)

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
alloc lambdaForm heap = (addr, heap')
  where
    ([addr], heap') = allocMany [lambdaForm] heap

-- | Store many values in the heap at unused addresses, and return them
-- in input order.
allocMany :: [Closure] -> Heap -> ([MemAddr], Heap)
allocMany closures (Heap heap) = (addrs, heap')
  where
    addrs = takeMatchingLength
        (L.filter (\i -> M.notMember i heap) (map MemAddr [0..]))
        closures
    heap' = Heap (heap <> M.fromList (zip addrs closures))

-- | All addresses allocated on the heap.
addresses :: Heap -> Set MemAddr
addresses (Heap h) = M.keysSet h

-- | Take as many elements from one list as there are in another.
--
-- This is just a lazier version of @\xs ys -> take (length ys) xs@.
takeMatchingLength :: [a] -> [b] -> [a]
takeMatchingLength xs ys = zipWith const xs ys
