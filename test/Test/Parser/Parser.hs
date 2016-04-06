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
        (Binds [("a", LambdaForm [] Update []
                          (Lit 1))])

    , shouldParseToSuccess "a = () \\n () -> Maybe (a, 1#)"
        (Binds [("a", LambdaForm [] NoUpdate []
                          (AppC "Maybe"
                                 [AtomVar "a" , AtomLit 1]))])

   , shouldParseToSuccess
        (unlines ["a = () \\n () ->"
                 , "    let y = (a) \\u (x) -> Foo (x)"
                 , "    in Con (y)" ])
       (Binds [("a", LambdaForm [] NoUpdate []
                         (Let NonRecursive (Binds
                             [("y", LambdaForm ["a"] Update ["x"]
                                        (AppC "Foo" [AtomVar "x"]))])
                             (AppC "Con" [AtomVar "y"])))])
    ]

shouldParseToSuccess :: String -> Binds -> TestTree
shouldParseToSuccess input output = shouldParseTo input (Right (Program output))

shouldParseTo :: String -> Either String Program -> TestTree
shouldParseTo input output =
    testCase input
        (let actual = parse (T.pack input)
             expected = output
        in assertEqual "" expected actual )
