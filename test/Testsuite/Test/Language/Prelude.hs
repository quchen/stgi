module Test.Language.Prelude (tests) where



import           Test.Tasty

import qualified Test.Language.Prelude.Bool     as Bool
import qualified Test.Language.Prelude.Bool     as Bool
import qualified Test.Language.Prelude.Function as Function
import qualified Test.Language.Prelude.List     as List



tests :: TestTree
tests = testGroup "Prelude"
    [ List.tests
    , Bool.tests
    , Function.tests
    , Bool.tests ]
