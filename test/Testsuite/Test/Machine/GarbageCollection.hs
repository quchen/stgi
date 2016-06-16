{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Machine.GarbageCollection (tests) where



import           Control.DeepSeq
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import           Data.Monoid
import qualified Data.Set           as S
import           Data.Text          (Text)
import qualified Data.Text          as T

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
    [ gcTests "Tri-state tracing" triStateTracing
    , gcTests "Two space copying" twoSpaceCopying ]

gcTests :: Text -> GarbageCollectionAlgorithm -> TestTree
gcTests name algorithm = testGroup (T.unpack name)
    [ test algorithm | test <- [ splitHeapTest
                               , fibonacciSumTest ]]

prettyIndented :: Pretty a => a -> Text
prettyIndented = T.unlines . map ("    " <>) . T.lines . prettyprint

splitHeapTest :: GarbageCollectionAlgorithm -> TestTree
splitHeapTest algorithm = testGroup "Split heap in dead/alive"
    [ deadAddressesFound
    , deadAndAliveContainAll ]
  where
    addr ~> closure = (MemAddr addr, HClosure closure)
    dirtyHeap = Heap
        [ 0 ~> Closure [stg| \(cyclic used1) -> Used cyclic used1 |]
                       [Addr (MemAddr addr) | addr <- [4,1] ]
        , 1 ~> Closure [stg| \(used2 prim) -> Used used2 prim |]
                       [Addr (MemAddr 2), PrimInt 1]
        , 2 ~> Closure [stg| \ -> Used   |] []
        , 3 ~> Closure [stg| \ -> Unused |] []
        , 4 ~> Closure [stg| \(cyclic) -> Used cyclic |]
                       [Addr (MemAddr 4)] ]
    globals = Globals [("main", Addr (MemAddr 0))]
    expectedDead = [MemAddr 3]

    dummyState = StgState
        { stgCode    = ReturnInt 1
        , stgStack   = mempty
        , stgHeap    = dirtyHeap
        , stgGlobals = globals
        , stgSteps   = 0
        , stgInfo    = Info GarbageCollection [] }

    errorMsg = T.unlines
        [ "Globals:"
        , prettyIndented globals
        , "Dirty heap:"
        , prettyIndented dirtyHeap
        , "Clean heap:"
        , prettyIndented cleanHeap ]

    (deadAddrs, _forwards, StgState{stgHeap = cleanHeap})
      = splitHeapWith algorithm dummyState

    deadAddressesFound = testCase "Dead addresses are found" test
      where
        test = assertEqual (T.unpack errorMsg)
                           expectedDead
                           deadAddrs

    deadAndAliveContainAll = testCase "Heap shrinks by number of dead addresses" test
      where
        expectedNewHeapSize = let Heap old = dirtyHeap
                              in M.size old - S.size deadAddrs
        actualNewHeapSize = let Heap new = cleanHeap
                            in M.size new
        test = assertEqual (T.unpack errorMsg)
                           expectedNewHeapSize
                           actualNewHeapSize


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
    states = NE.take 1e3 (evalsUntil (RunForMaxSteps 1e10)
                                     (HaltIf (const False))
                                     (PerformGc (const (Just algorithm)))
                                     prog )
    test = rnf states `seq` pure ()
