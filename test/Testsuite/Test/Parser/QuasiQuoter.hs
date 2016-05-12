{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Parser.QuasiQuoter (tests) where



import Test.Tasty
import Test.Tasty.HUnit

import Stg.Parser



tests :: TestTree
tests = testGroup "Quasiquoter"
    [ testCase "Simple definition"
        (let actual = Right [stg| f = () \n () -> Hello () |]
             expected = parse "f = () \\n () -> Hello ()"
         in assertEqual "" expected actual)
    ]
