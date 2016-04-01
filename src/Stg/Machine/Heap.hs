module Stg.Machine.Heap (
    heapLookup,
    heapUpdate,
    heapAlloc,
    heapAllocMany,
) where



import qualified Data.Map          as M

import           Stg.Machine.Types



heapLookup :: MemAddr -> Heap -> Maybe Closure
heapLookup addr (Heap heap) = M.lookup addr heap

heapUpdate :: MemAddr -> Closure -> Heap -> Heap
heapUpdate addr cl (Heap h) = Heap (M.adjust (const cl) addr h)

heapAlloc :: Closure -> Heap -> (MemAddr, Heap)
heapAlloc lambdaForm (Heap h) = (addr, heap')
  where
    addr = MemAddr (case M.maxViewWithKey h of
        Just ((MemAddr x,_),_) -> x+1
        Nothing                -> 0 )
    heap' = Heap (M.insert addr lambdaForm h)

heapAllocMany :: [Closure] -> Heap -> ([MemAddr], Heap)
heapAllocMany [] heap = ([], heap)
heapAllocMany (cl:cls) heap =
    let (addr, heap') = heapAlloc cl heap
        (addrs, heap'') = heapAllocMany cls heap'
    in (addr:addrs, heap'')
