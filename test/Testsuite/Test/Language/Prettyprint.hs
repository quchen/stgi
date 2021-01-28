{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Test.Language.Prettyprint (tests) where



import           Data.Bifunctor
import           Data.Text      (Text)
import qualified Data.Text      as T

import Stg.Language.Prettyprint
import Stg.Parser.Parser        as Parser

import Test.Orphans             ()
import Test.QuickCheck.Property
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.QuickCheck    as QC
import Test.Tasty.SmallCheck    as SC



tests :: TestTree
tests = localOption (QuickCheckMaxSize 3)
    ( testGroup "Plain prettyprinter is inverse of parser"
        [ inverseOfParserQC "Full program"      Parser.program
        , inverseOfParserQC "Bindings"          Parser.binds
        , inverseOfParserQC "Lambda form"       Parser.lambdaForm
        , inverseOfParserQC "Expression"        Parser.expr
        , inverseOfParserQC "Case alternatives" Parser.alts
        , inverseOfParserSC "Literal"           Parser.literal
        , inverseOfParserSC "Primop"            Parser.primOp
        , inverseOfParserQC "Atom"              Parser.atom ])

inverseOfParserQC
    :: (Arbitrary ast, Show ast, Eq ast, PrettyStgi ast)
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
    :: (Serial IO ast, Show ast, Eq ast, PrettyStgi ast)
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
    :: PrettyStgi ast
    => StgParser ast
    -> ast
    -> Either Text ast
parserRoundtrip parser = first renderPlain . parse parser . renderPlain . prettyStgi

prettySuccess :: PrettyStgi ast => ast -> ast -> Text
prettySuccess inputAst parsedAst =
    T.unlines [ "Input AST:"
              , renderPlain (prettyStgi inputAst)
              , "Parsed, parser inverse printed AST:"
              , renderPlain (prettyStgi parsedAst) ]

prettyFailure :: PrettyStgi ast => ast -> Text -> Text
prettyFailure inputAst err =
    T.unlines [ "Input AST:"
              , renderPlain (prettyStgi inputAst)
              , "Parse error:"
              , err ]
