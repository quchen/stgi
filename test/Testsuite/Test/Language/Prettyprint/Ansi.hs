{-# LANGUAGE OverloadedStrings #-}

module Test.Language.Prettyprint.Ansi (tests) where



import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Text.Megaparsec.Text
import           Text.PrettyPrint.ANSI.Leijen  (plain)

import           Stg.Language.Prettyprint
import           Stg.Language.Prettyprint.Ansi
import           Stg.Parser.Parser             as Parser

import           Test.Orphans                  ()



tests :: TestTree
tests = testGroup "ANSI terminal printer stripped off styles is parser inverse"
    [ reducesToParserInverse "Full program"      Parser.program
    , reducesToParserInverse "Bindings"          Parser.binds
    , reducesToParserInverse "Lambda form"       Parser.lambdaForm
    , reducesToParserInverse "Expression"        Parser.expr
    , reducesToParserInverse "Case alternatives" Parser.alts
    , reducesToParserInverse "Literal"           Parser.literal -- TODO: Use Smallcheck for this
    , reducesToParserInverse "Primop"            Parser.primOp  -- TODO: Use Smallcheck for this
    , reducesToParserInverse "Atom"              Parser.atom ]

reducesToParserInverse
    :: (Arbitrary ast, Show ast, Eq ast, PrettyAnsi ast)
    => Text
    -> Parser ast
    -> TestTree
reducesToParserInverse testName parser = testProperty (T.unpack testName) test
  where
    test inputAst =
        let prettyprinted = prettyAnsi inputAst
            prettyAsPlainText = prettyprint (plain prettyprinted)
            parsed = parse parser prettyAsPlainText
        in case parsed of
            Left err ->
                counterexample (T.unpack (prettyFailure inputAst err))
                               (property failed)
            Right parsedAst ->
                counterexample (T.unpack (prettySuccess inputAst parsedAst))
                               (inputAst == parsedAst)

    prettySuccess inputAst parsedAst =
        T.unlines [ "Input AST:"
                  , prettyprintAnsi inputAst
                  , "Parsed, parser inverse printed AST:"
                  , prettyprint parsedAst ]

    prettyFailure inputAst err =
        T.unlines [ "Input AST:"
                  , prettyprintAnsi inputAst
                  , "Parse error:"
                  , err ]
