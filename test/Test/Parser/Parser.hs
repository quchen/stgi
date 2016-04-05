{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.Parser (tests) where


import qualified Data.Text         as T
import           Test.Orphans      ()
import           Test.Tasty
import           Test.Tasty.HUnit

import           Stg.Language
import           Stg.Parser.Parser

tests :: TestTree
tests = testGroup "Parser" [exampleParses]

exampleParses :: TestTree
exampleParses = testGroup "Successful parses"
    [ shouldParseToSuccess "a = () \\u () -> 1#"
        (Binds [(Var "a",LambdaForm [] Update [] (Lit (Literal 1)))]) ]

shouldParseToSuccess :: String -> Binds -> TestTree
shouldParseToSuccess input output = shouldParseTo input (Right (Program output))

shouldParseTo :: String -> Either String Program -> TestTree
shouldParseTo input output =
    testCase input
        (let actual = parse (T.pack input)
             expected = output
        in assertEqual "" expected actual )
