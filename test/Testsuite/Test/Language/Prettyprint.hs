module Test.Language.Prettyprint (tests) where



import           Test.Tasty

import qualified Test.Language.Prettyprint.Ansi          as PrettyAnsi
import qualified Test.Language.Prettyprint.ParserInverse as PrettyInv



tests :: TestTree
tests = testGroup "Prettyprinter" [PrettyInv.tests, PrettyAnsi.tests]
