module Test.Language (tests) where



import           Test.Tasty

import qualified Test.Language.Prelude     as StgPrelude
import qualified Test.Language.Prettyprint as Pretty



tests :: TestTree
tests = testGroup "Language"
    [ Pretty.tests
    , StgPrelude.tests
    ]
