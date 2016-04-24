{-# LANGUAGE OverloadedStrings #-}

module Test.Language.Prettyprint.ParserInverse (tests) where



import           Data.Text                (Text)
import qualified Data.Text                as T
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Text.Megaparsec.Text

import           Stg.Language.Prettyprint
import           Stg.Parser.Parser        as Parser

import           Test.Orphans             ()



tests :: TestTree
tests = testGroup "Inverse of parser"
    [ inverseOfParser "Full program"      Parser.program
    , inverseOfParser "Bindings"          Parser.binds
    , inverseOfParser "Lambda form"       Parser.lambdaForm
    , inverseOfParser "Expression"        Parser.expr
    , inverseOfParser "Case alternatives" Parser.alts
    , inverseOfParser "Literal"           Parser.literal
    , inverseOfParser "Primop"            Parser.primOp
    , inverseOfParser "Atom"              Parser.atom ]

inverseOfParser
    :: (Arbitrary ast, Show ast, Eq ast, PrettyParserInverse ast)
    => Text
    -> Parser ast
    -> TestTree
inverseOfParser testName parser = testProperty (T.unpack testName) test
  where
    test inputAst =
        let prettyprinted = prettyprintParserInverse inputAst
            parsed = parse parser prettyprinted
        in case parsed of
            Left err ->
                counterexample (T.unpack (prettyFailure inputAst err))
                               (property failed)
            Right parsedAst ->
                counterexample (T.unpack (prettySuccess inputAst parsedAst))
                               (inputAst == parsedAst)

    prettySuccess inputAst parsedAst =
        T.unlines [ "Input AST:"
                  , prettyprintParserInverse inputAst
                  , "Parsed AST:"
                  , prettyprintParserInverse parsedAst ]

    prettyFailure inputAst err =
        T.unlines [ "Input AST:"
                  , prettyprintParserInverse inputAst
                  , "Parse error:"
                  , err ]
