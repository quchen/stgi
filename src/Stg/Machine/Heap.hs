module Stg.Machine.Heap (
    lookup,
    update,
    alloc,
    allocMany,
) where



import qualified Data.Map          as M
import           Prelude           hiding (lookup)

import           Stg.Machine.Types



lookup :: MemAddr -> Heap -> Maybe Closure
lookup addr (Heap heap) = M.lookup addr heap

update :: MemAddr -> Closure -> Heap -> Heap
update addr cl (Heap h) = Heap (M.adjust (const cl) addr h)

alloc :: Closure -> Heap -> (MemAddr, Heap)
alloc lambdaForm (Heap h) = (addr, heap')
  where
    addr = MemAddr (case M.maxViewWithKey h of
        Just ((MemAddr x,_),_) -> x+1
        Nothing                -> 0 )
    heap' = Heap (M.insert addr lambdaForm h)

allocMany :: [Closure] -> Heap -> ([MemAddr], Heap)
allocMany [] heap = ([], heap)
allocMany (cl:cls) heap =
    let (addr, heap') = alloc cl heap
        (addrs, heap'') = allocMany cls heap'
    in (addr:addrs, heap'')
