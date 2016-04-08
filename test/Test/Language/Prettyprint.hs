{-# LANGUAGE OverloadedStrings #-}

module Test.Language.Prettyprint (tests) where



import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Text.Megaparsec.Text
import           Text.PrettyPrint.ANSI.Leijen

import           Stg.Language.Prettyprint
import qualified Stg.Parser.Parser            as Parse

import           Test.Orphans                 ()



tests :: TestTree
tests = testGroup "Prettyprinter is inverse of parser"
    [ inverseOfParser "Full program"           Parse.program
    , inverseOfParser "Bindings"               Parse.binds
    , inverseOfParser "Lambda form"            Parse.lambdaForm
    , inverseOfParser "Expression"             Parse.expr
    , inverseOfParser "Case alternatives"      Parse.alts
    , inverseOfParser "Algebraic alternatives" Parse.algebraicAlts
    , inverseOfParser "Primitive alternatives" Parse.primitiveAlts
    , inverseOfParser "Algebraic alternative"  Parse.algebraicAlt
    , inverseOfParser "Primitive alternative"  Parse.primitiveAlt
    , inverseOfParser "Default alternative"    Parse.defaultAlt
    , inverseOfParser "Literal"                Parse.literal
    , inverseOfParser "Primop"                 Parse.primOp
    , inverseOfParser "Atom"                   Parse.atom ]

inverseOfParser
    :: (Arbitrary ast, Pretty ast, Show ast, Eq ast)
    => Text
    -> Parser ast
    -> TestTree
inverseOfParser testName parser = small (testProperty (T.unpack testName) test)
  where
    small = localOption (QuickCheckMaxSize 2) -- Higher numbers sometimes let
                                              -- the test AST *explode*
    test inputAst =
        let prettyprinted = prettyprint inputAst
            parsed = Parse.parse parser prettyprinted
        in case parsed of
            Left err ->
                counterexample (T.unpack (prettyFailure inputAst err))
                               (property failed)
            Right parsedAst ->
                counterexample (T.unpack (prettySuccess inputAst parsedAst))
                               (inputAst == parsedAst)

    prettySuccess inputAst parsedAst =
        T.unlines [ "Input AST:"
                  , prettyprint inputAst
                  , "Parsed AST:"
                  , prettyprint parsedAst ]

    prettyFailure inputAst err =
        T.unlines [ "Input AST:"
                  , prettyprint inputAst
                  , "Parse error:"
                  , err ]
