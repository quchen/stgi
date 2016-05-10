{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Test.Language.Prettyprint.Ansi (tests) where



import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Test.QuickCheck.Property
import           Test.SmallCheck.Series
import           Test.Tasty
import           Test.Tasty.QuickCheck        as QC
import           Test.Tasty.SmallCheck        as SC
import           Text.Megaparsec.Text
import           Text.PrettyPrint.ANSI.Leijen (plain)

import           Stg.Language.Prettyprint
import           Stg.Parser.Parser            as Parser

import           Test.Orphans                 ()



tests :: TestTree
tests = testGroup "ANSI terminal printer stripped off styles is parser inverse"
    [ reducesToParserInverseQC "Full program"      Parser.program
    , reducesToParserInverseQC "Bindings"          Parser.binds
    , reducesToParserInverseQC "Lambda form"       Parser.lambdaForm
    , reducesToParserInverseQC "Expression"        Parser.expr
    , reducesToParserInverseQC "Case alternatives" Parser.alts
    , reducesToParserInverseSC "Literal"           Parser.literal
    , reducesToParserInverseSC "Primop"            Parser.primOp
    , reducesToParserInverseQC "Atom"              Parser.atom ]

reducesToParserInverseQC
    :: (Arbitrary ast, Show ast, Eq ast, PrettyAnsi ast)
    => Text
    -> Parser ast
    -> TestTree
reducesToParserInverseQC testName parser
  = QC.testProperty (T.unpack testName) (\inputAst ->
        case parserRoundtrip parser inputAst of
            Left err ->
                counterexample (T.unpack (prettyFailure inputAst err))
                               (property failed)
            Right parsedAst ->
                counterexample (T.unpack (prettySuccess inputAst parsedAst))
                               (inputAst == parsedAst) )

reducesToParserInverseSC
    :: (Serial IO ast, Show ast, Eq ast, PrettyAnsi ast)
    => Text
    -> Parser ast
    -> TestTree
reducesToParserInverseSC testName parser
  = SC.testProperty (T.unpack testName) (SC.forAll (\inputAst ->
        case parserRoundtrip parser inputAst of
            Left err ->
                Left (T.unpack (prettyFailure inputAst err))
            Right parsedAst
                | inputAst == parsedAst -> Right ("" :: String)
                | otherwise -> Left (T.unpack (prettySuccess inputAst parsedAst)) ))

parserRoundtrip
    :: PrettyAnsi ast
    => Parser ast
    -> ast
    -> Either Text ast
parserRoundtrip parser inputAst =
    let prettyprinted = prettyAnsi inputAst
        prettyAsPlainText = prettyprint (plain prettyprinted)
   in parse parser prettyAsPlainText

prettySuccess :: PrettyAnsi ast => ast -> ast -> Text
prettySuccess inputAst parsedAst =
    T.unlines [ "Input AST:"
              , prettyprintAnsi inputAst
              , "Parsed, parser inverse printed AST:"
              , prettyprint parsedAst ]

prettyFailure :: PrettyAnsi ast => ast -> Text -> Text
prettyFailure inputAst err =
    T.unlines [ "Input AST:"
              , prettyprintAnsi inputAst
              , "Parse error:"
              , err ]
