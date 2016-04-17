module Test.Language.Prettyprint (tests) where



import           Test.Tasty

import qualified Test.Language.Prettyprint.ParserInverse     as PrettyInv
import qualified Test.Language.Prettyprint.ParserInverseAnsi as PrettyAnsi



tests :: TestTree
tests = testGroup "Prettyprinter" [PrettyInv.tests, PrettyAnsi.tests]
