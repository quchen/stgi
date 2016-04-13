module Test.Machine.Heap (tests) where



import           Test.Tasty
import           Test.Tasty.HUnit

import           Stg.Machine.Heap


tests :: TestTree
tests = testGroup "Heap"
    [ testCase "Lookup single inserted item" (assertFailure "Implement me")
    , testCase "Insertion always increaes heap size" (assertFailure "Implement me")
    , testCase "Updating heap works" (assertFailure "Implement me")
    , testCase "Lookup many inserted items" (assertFailure "Implement me")
    ]
