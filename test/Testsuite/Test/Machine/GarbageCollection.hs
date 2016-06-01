{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Machine.GarbageCollection (tests) where



import           Control.DeepSeq
import qualified Data.Map        as M
import           Data.Monoid
import qualified Data.Set        as S
import           Data.Text       (Text)
import qualified Data.Text       as T

import           Stg.Language.Prettyprint
import           Stg.Machine
import           Stg.Machine.GarbageCollection.Common
import           Stg.Machine.Types
import           Stg.Marshal
import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude                          as Stg

import Test.Orphans     ()
import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Garbage collection"
    [ gcTests "Tri-state tracing" triStateTracing ]

gcTests :: Text -> GarbageCollectionAlgorithm -> TestTree
gcTests name algorithm = testGroup (T.unpack name)
    [ test algorithm | test <- [ splitHeapTest
                               , fibonacciSumTest ]]

prettyIndented :: Pretty a => a -> Text
prettyIndented = T.unlines . map ("    " <>) . T.lines . prettyprint

splitHeapTest :: GarbageCollectionAlgorithm -> TestTree
splitHeapTest algorithm = testGroup "Split heap in dead/alive"
    [ unusedIsCollected
    , usedIsNotCollected
    , heapSplit ]
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
        , stgSteps   = 0
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
        (Dead (Heap actualDead), Alive cleanHeap) = splitHeapWith algorithm dummyState
        test = assertEqual (T.unpack (errorMsg cleanHeap))
                           expectedDead
                           (M.keysSet actualDead)

    usedIsNotCollected = testCase "Alives are kept" test
      where
        expectedHeap = let Heap h = dirtyHeap
                       in Heap (M.delete (MemAddr 3) h)
        (_dead, Alive cleanHeap) = splitHeapWith algorithm dummyState
        test = assertEqual (T.unpack (errorMsg cleanHeap))
                           expectedHeap
                           cleanHeap

    heapSplit = testCase "dead+alive contain all previous addresses" test
      where
        expected = dirtyHeap
        actual = dead <> cleanHeap
        (Dead dead, Alive cleanHeap) = splitHeapWith algorithm dummyState
        test = assertEqual (T.unpack (errorMsg cleanHeap))
                           expected
                           actual


fibonacciSumTest :: GarbageCollectionAlgorithm -> TestTree
fibonacciSumTest algorithm = testCase "Long-running program" test
  where
    -- This program choked on the new copying GC (ran into an infinite loop),
    -- so it is added as a test case. It's much rather a sanity test than a
    -- minimal example displaying the actual issue, however.
    source = mconcat
            [ Stg.add
            , toStg "zero" (0 :: Int)
            , Stg.foldl'
            , Stg.zipWith ] <> [stg|

        flipConst = \x y -> y;
        main = \ =>
            letrec
                fibo = \ =>
                    letrec
                        fib0 = \(fib1) -> Cons zero fib1;
                        fib1 = \(fib2) =>
                            let one = \ -> Int# 1#
                            in Cons one fib2;
                        fib2 = \(fib0 fib1) => zipWith add fib0 fib1
                    in fib0
            in foldl' flipConst zero fibo
        |]
    prog = initialState "main" source
    states = take 1e3 (evalsUntil (RunForMaxSteps 1e10)
                                   (HaltIf (const False))
                                   (PerformGc (const (Just algorithm)))
                                   prog )
    test = rnf states `seq` pure ()
