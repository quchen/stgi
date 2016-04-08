{-# LANGUAGE OverloadedStrings #-}

module Test.Language.Prettyprint (tests) where



import qualified Data.Text                as T
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Stg.Language.Prettyprint
import           Stg.Parser.Parser

import           Test.Orphans             ()



tests :: TestTree
tests = testGroup "Prettyprinter" [inverseOfParser]

inverseOfParser :: TestTree
inverseOfParser = testProperty "inverse of parser" test
  where
    test inputAst =
        let prettyprinted = prettyprint 80 inputAst
            parsed = parse prettyprinted
        in case parsed of
            Left err ->
                counterexample (T.unpack (prettyFailure inputAst err))
                               (property failed)
            Right parsedAst ->
                counterexample (T.unpack (prettySuccess inputAst parsedAst))
                               (inputAst == parsedAst)

    prettySuccess inputAst parsedAst =
        T.unlines [ "Input AST:"
                  , prettyprint 80 inputAst
                  , "Parsed AST:"
                  , prettyprint 80 parsedAst ]

    prettyFailure inputAst err =
        T.unlines [ "Input AST:"
                  , prettyprint 80 inputAst
                  , "Parse error:"
                  , err ]
