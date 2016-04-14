module Test.Machine (tests) where



import           Test.Tasty

import qualified Test.Machine.Heap as Heap



tests :: TestTree
tests = testGroup "STG machine" [Heap.tests]
