{-# LANGUAGE OverloadedStrings #-}

module Test.Language.Prettyprint.ParserInverse (tests) where



import           Data.Text                (Text)
import qualified Data.Text                as T
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Text.Megaparsec.Text

import           Stg.Language.Prettyprint
import           Stg.Parser.Parser

import           Test.Orphans             ()



tests :: TestTree
tests = testGroup "Inverse of parser"
    [ inverseOfParser "Full program"           program
    , inverseOfParser "Bindings"               binds
    , inverseOfParser "Lambda form"            lambdaForm
    , inverseOfParser "Expression"             expr
    , inverseOfParser "Case alternatives"      alts
    , inverseOfParser "Algebraic alternatives" algebraicAlts
    , inverseOfParser "Primitive alternatives" primitiveAlts
    , inverseOfParser "Algebraic alternative"  algebraicAlt
    , inverseOfParser "Primitive alternative"  primitiveAlt
    , inverseOfParser "Default alternative"    defaultAlt
    , inverseOfParser "Literal"                literal
    , inverseOfParser "Primop"                 primOp
    , inverseOfParser "Atom"                   atom ]

inverseOfParser
    :: (Arbitrary ast, Show ast, Eq ast, PrettyParserInverse ast)
    => Text
    -> Parser ast
    -> TestTree
inverseOfParser testName parser = testProperty (T.unpack testName) test
  where
    test inputAst =
        let prettyprinted = prettyParserInverse inputAst
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
                  , prettyParserInverse inputAst
                  , "Parsed AST:"
                  , prettyParserInverse parsedAst ]

    prettyFailure inputAst err =
        T.unlines [ "Input AST:"
                  , prettyParserInverse inputAst
                  , "Parse error:"
                  , err ]
