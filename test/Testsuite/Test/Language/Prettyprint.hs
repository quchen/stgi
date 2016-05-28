{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Test.Language.Prettyprint (tests) where



import           Data.Bifunctor
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Test.QuickCheck.Property
import           Test.SmallCheck.Series

import Stg.Language.Prettyprint
import Stg.Parser.Parser        as Parser

import Test.Orphans          ()
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC



tests :: TestTree
tests = localOption (QuickCheckMaxSize 3)
    ( testGroup "Plain prettyprinter is inverse of parser"
        [ localOption (Timeout 5000000 "5 s") $ inverseOfParserQC "Atom"              Parser.atom
        , localOption (Timeout 5000000 "5 s") $ inverseOfParserSC "Primop"            Parser.primOp
        , localOption (Timeout 5000000 "5 s") $ inverseOfParserSC "Literal"           Parser.literal
        , localOption (Timeout 5000000 "5 s") $ inverseOfParserQC "Case alternatives" Parser.alts
        , localOption (Timeout 5000000 "5 s") $ inverseOfParserQC "Expression"        Parser.expr
        , localOption (Timeout 5000000 "5 s") $ inverseOfParserQC "Lambda form"       Parser.lambdaForm
        , localOption (Timeout 5000000 "5 s") $ inverseOfParserQC "Bindings"          Parser.binds
        , localOption (Timeout 5000000 "5 s") $ inverseOfParserQC "Full program"      Parser.program ])

inverseOfParserQC
    :: (Arbitrary ast, Show ast, Eq ast, Pretty ast)
    => Text
    -> StgParser ast
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
    :: (Serial IO ast, Show ast, Eq ast, Pretty ast)
    => Text
    -> StgParser ast
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
    :: Pretty ast
    => StgParser ast
    -> ast
    -> Either Text ast
parserRoundtrip parser = first prettyprintPlain . parse parser . prettyprintPlain

prettySuccess :: Pretty ast => ast -> ast -> Text
prettySuccess inputAst parsedAst =
    T.unlines [ "Input AST:"
              , prettyprintPlain inputAst
              , "Parsed, parser inverse printed AST:"
              , prettyprintPlain parsedAst ]

prettyFailure :: Pretty ast => ast -> Text -> Text
prettyFailure inputAst err =
    T.unlines [ "Input AST:"
              , prettyprintPlain inputAst
              , "Parse error:"
              , err ]
