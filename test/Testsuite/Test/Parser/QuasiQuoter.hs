{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Parser.QuasiQuoter (tests) where



import Data.Bifunctor

import Stg.Language.Prettyprint
import Stg.Parser.Parser        as Parser
import Stg.Parser.QuasiQuoter   as QQ

import Test.Tasty
import Test.Tasty.HUnit



tests :: TestTree
tests = testGroup "Quasiquoter"
    [ testCase "Simple definition"
        (let actual = Right [stg| f = \ -> Hello |]
             expected = first renderPlain (parse Parser.program "f = \\ -> Hello")
         in assertEqual "" expected actual)
    ]
