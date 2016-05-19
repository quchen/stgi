{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Machine.GarbageCollection (tests) where



import qualified Data.Map         as M
import           Data.Monoid
import qualified Data.Set         as S
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Test.Tasty
import           Test.Tasty.HUnit

import Stg.Language.Prettyprint
import Stg.Machine.GarbageCollection
import Stg.Machine.Types
import Stg.Parser.QuasiQuoter

import Test.Orphans ()



tests :: TestTree
tests = testGroup "Garbage collection"
    [ splitHeapTest
    ]

prettyIndented :: Pretty a => a -> Text
prettyIndented = T.unlines . map ("    " <>) . T.lines . prettyprint

splitHeapTest :: TestTree
splitHeapTest = testGroup "Split heap in dead/alive"
    [ unusedIsCollected
    , usedIsNotCollected
    , heapSplit
    ]
  where
    (~>) = (,)
    dirtyHeap = Heap
        [ MemAddr 0 ~> HClosure (Closure [stg| \(used1) -> Used used1 |]
                                [Addr (MemAddr 1)] )
        , MemAddr 1 ~> HClosure (Closure [stg| \(used2 prim) -> Used used2 prim |]
                                [Addr (MemAddr 2), PrimInt 1] )
        , MemAddr 2 ~> HClosure (Closure [stg| \ -> Used   |] [])
        , MemAddr 3 ~> HClosure (Closure [stg| \ -> Unused |] []) ]
    globals = Globals
        [ "main"  ~> Addr (MemAddr 0) ]

    dummyState = StgState
        { stgCode    = ReturnInt 1
        , stgStack   = mempty
        , stgHeap    = dirtyHeap
        , stgGlobals = globals
        , stgTicks   = 0
        , stgInfo    = Info GarbageCollection [] }

    errorMsg cleanHeap = T.unlines
        [ "Globals:"
        , prettyIndented globals
        , "Dirty heap:"
        , prettyIndented dirtyHeap
        , "Clean heap:"
        , prettyIndented cleanHeap ]

    unusedIsCollected = testCase "Dead address is found" test
      where
        expectedDead = S.singleton (MemAddr 3)
        (Dead (Heap actualDead), Alive cleanHeap) = splitHeap dummyState
        test = assertEqual (T.unpack (errorMsg cleanHeap))
                           expectedDead
                           (M.keysSet actualDead)

    usedIsNotCollected = testCase "Alives are kept" test
      where
        expectedHeap = let Heap h = dirtyHeap
                       in Heap (M.delete (MemAddr 3) h)
        (_dead, Alive cleanHeap) = splitHeap dummyState
        test = assertEqual (T.unpack (errorMsg cleanHeap))
                           expectedHeap
                           cleanHeap

    heapSplit = testCase "dead+alive contain all previous addresses" test
      where
        expected = dirtyHeap
        actual = dead <> cleanHeap
        (Dead dead, Alive cleanHeap) = splitHeap dummyState
        test = assertEqual (T.unpack (errorMsg cleanHeap))
                           expected
                           actual
