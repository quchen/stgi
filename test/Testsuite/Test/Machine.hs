module Test.Machine (tests) where



import           Test.Tasty

import qualified Test.Machine.Evaluate as Evaluate
import qualified Test.Machine.Heap     as Heap



tests :: TestTree
tests = testGroup "Machine" [Heap.tests, Evaluate.tests]
