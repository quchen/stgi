module Test.Prelude (tests) where



import Test.Tasty

import qualified Test.Prelude.Bool     as Bool
import qualified Test.Prelude.Function as Function
import qualified Test.Prelude.List     as List
import qualified Test.Prelude.Maybe    as Maybe
import qualified Test.Prelude.Number   as Number
import qualified Test.Prelude.Tuple    as Tuple



tests :: TestTree
tests = testGroup "Prelude"
    [ Bool.tests
    , Function.tests
    , List.tests
    , Maybe.tests
    , Number.tests
    , Tuple.tests ]
