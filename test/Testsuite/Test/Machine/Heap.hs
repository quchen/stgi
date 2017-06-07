module Test.Machine.Heap (tests) where



import Data.Text.Prettyprint.Doc

import           Stg.Language.Prettyprint
import qualified Stg.Machine.Heap         as Heap
import           Stg.Machine.Types

import Test.Orphans          ()
import Test.Tasty
import Test.Tasty.QuickCheck



tests :: TestTree
tests = testGroup "Heap"
    [ testProperty "Lookup single inserted item"
        (\closure heap ->
            let (addr, heap') = Heap.alloc closure heap
            in Heap.lookup addr heap' === Just closure )
    , testProperty "Heap grows with allocated elements"
        (\heap closures ->
            let (_addrs, heap') = Heap.allocMany closures heap
            in Heap.size heap' === Heap.size heap + length closures )
    , testProperty "Update heap overwrites old values"
        (\closure1 closure2 heap ->
            let (addr1, heap1) = Heap.alloc closure1 heap
                heap2 = Heap.update (Mapping addr1 closure2) heap1
            in counterexample (show (prettyStgi heap2 :: Doc StgiAnn)
                                <> "\ndoes not contain "
                                <> show (prettyStgi closure2 :: Doc StgiAnn))
                              (Heap.lookup addr1 heap2 == Just closure2) )

    , testProperty "Lookup many inserted items" (\closures heap ->
        let (addrs, heap') = Heap.allocMany closures heap
        in traverse (\addr -> Heap.lookup addr heap') addrs
           ==
           Just closures
        )
    ]
