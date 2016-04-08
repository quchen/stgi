module Test.Parser (tests) where



import           Test.Tasty

import qualified Test.Parser.Parser      as Parser
import qualified Test.Parser.QuasiQuoter as QuasiQuoter



tests :: TestTree
tests = testGroup "Parser" [Parser.tests, QuasiQuoter.tests]
