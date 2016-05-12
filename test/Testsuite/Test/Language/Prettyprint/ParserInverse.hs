{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Test.Language.Prettyprint.ParserInverse (tests) where



import           Data.Text                (Text)
import qualified Data.Text                as T
import           Test.QuickCheck.Property
import           Test.SmallCheck.Series
import           Test.Tasty
import           Test.Tasty.QuickCheck    as QC
import           Test.Tasty.SmallCheck    as SC
import           Text.Megaparsec.Text

import Stg.Language.Prettyprint
import Stg.Parser.Parser        as Parser

import Test.Orphans ()



tests :: TestTree
tests = testGroup "Inverse of parser"
    [ inverseOfParserQC "Full program"      Parser.program
    , inverseOfParserQC "Bindings"          Parser.binds
    , inverseOfParserQC "Lambda form"       Parser.lambdaForm
    , inverseOfParserQC "Expression"        Parser.expr
    , inverseOfParserQC "Case alternatives" Parser.alts
    , inverseOfParserSC "Literal"           Parser.literal
    , inverseOfParserSC "Primop"            Parser.primOp
    , inverseOfParserQC "Atom"              Parser.atom ]

inverseOfParserQC
    :: (Arbitrary ast, Show ast, Eq ast, PrettyParserInverse ast)
    => Text
    -> Parser ast
    -> TestTree
inverseOfParserQC testName parser
  = QC.testProperty (T.unpack testName) (\inputAst ->
        case parserRoundtrip parser inputAst of
            Left err ->
                counterexample (T.unpack (prettyFailure inputAst err))
                               (property failed)
            Right parsedAst ->
                counterexample (T.unpack (prettySuccess inputAst parsedAst))
                               (inputAst == parsedAst) )

inverseOfParserSC
    :: (Serial IO ast, Show ast, Eq ast, PrettyParserInverse ast)
    => Text
    -> Parser ast
    -> TestTree
inverseOfParserSC testName parser
  = SC.testProperty (T.unpack testName) (SC.forAll (\inputAst ->
        case parserRoundtrip parser inputAst of
            Left err ->
                Left (T.unpack (prettyFailure inputAst err))
            Right parsedAst
                | inputAst == parsedAst -> Right ("" :: String)
                | otherwise -> Left (T.unpack (prettySuccess inputAst parsedAst)) ))

parserRoundtrip
    :: PrettyParserInverse ast
    => Parser ast
    -> ast
    -> Either Text ast
parserRoundtrip parser = parse parser . prettyprintParserInverse

prettySuccess :: PrettyParserInverse ast => ast -> ast -> Text
prettySuccess inputAst parsedAst =
    T.unlines [ "Input AST:"
              , prettyprintParserInverse inputAst
              , "Parsed, parser inverse printed AST:"
              , prettyprint parsedAst ]

prettyFailure :: PrettyParserInverse ast => ast -> Text -> Text
prettyFailure inputAst err =
    T.unlines [ "Input AST:"
              , prettyprintParserInverse inputAst
              , "Parse error:"
              , err ]
