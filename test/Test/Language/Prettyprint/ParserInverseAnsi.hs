{-# LANGUAGE OverloadedStrings #-}

module Test.Language.Prettyprint.ParserInverseAnsi (tests) where



import           Test.Tasty

import           Test.Orphans ()



tests :: TestTree
tests = testGroup "ANSI printer has to be verified manually" []
