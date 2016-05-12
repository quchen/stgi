module Test.Language.Prelude (tests) where



import Test.Tasty

import qualified Test.Language.Prelude.Bool     as Bool
import qualified Test.Language.Prelude.Function as Function
import qualified Test.Language.Prelude.List     as List
import qualified Test.Language.Prelude.Number   as Number
import qualified Test.Language.Prelude.Tuple    as Tuple



tests :: TestTree
tests = testGroup "Prelude"
    [ Bool.tests
    , Function.tests
    , List.tests
    , Number.tests
    , Tuple.tests ]
