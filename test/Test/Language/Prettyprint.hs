{-# LANGUAGE OverloadedStrings #-}

module Test.Language.Prettyprint (tests) where



import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Text.Megaparsec.Text
import           Text.PrettyPrint.ANSI.Leijen

import qualified Stg.Language.Prettyprint     as Prettyprint
import qualified Stg.Parser.Parser            as Parse

import           Test.Orphans                 ()



tests :: TestTree
tests = testGroup "Prettyprinter is inverse of parser"
    [ inverseOfParser "Full program"           Prettyprint.pprProgram       Parse.program
    , inverseOfParser "Bindings"               Prettyprint.pprBinds         Parse.binds
    , inverseOfParser "Lambda form"            Prettyprint.pprLambdaForm    Parse.lambdaForm
    , inverseOfParser "Expression"             Prettyprint.pprExpr          Parse.expr
    , inverseOfParser "Case alternatives"      Prettyprint.pprAlts          Parse.alts
    , inverseOfParser "Algebraic alternatives" Prettyprint.pprAlgebraicAlts Parse.algebraicAlts
    , inverseOfParser "Primitive alternatives" Prettyprint.pprPrimitiveAlts Parse.primitiveAlts
    , inverseOfParser "Algebraic alternative"  Prettyprint.pprAlgebraicAlt  Parse.algebraicAlt
    , inverseOfParser "Primitive alternative"  Prettyprint.pprPrimitiveAlt  Parse.primitiveAlt
    , inverseOfParser "Default alternative"    Prettyprint.pprDefaultAlt    Parse.defaultAlt
    , inverseOfParser "Literal"                Prettyprint.pprLiteral       Parse.literal
    , inverseOfParser "Primop"                 Prettyprint.pprPrimOp        Parse.primOp
    , inverseOfParser "Atom"                   Prettyprint.pprAtom          Parse.atom ]

inverseOfParser
    :: (Arbitrary ast, Show ast, Eq ast)
    => Text
    -> (Prettyprint.PrettyprinterDict Doc -> ast -> Doc)
    -> Parser ast
    -> TestTree
inverseOfParser testName prettyprinter parser = small (testProperty (T.unpack testName) test)
  where
    small = localOption (QuickCheckMaxSize 2) -- Higher numbers sometimes let
                                              -- the test AST *explode*
    test inputAst =
        let prettyprinted = Prettyprint.parserInverse prettyprinter inputAst
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
                  , Prettyprint.parserInverse prettyprinter inputAst
                  , "Parsed AST:"
                  , Prettyprint.parserInverse prettyprinter parsedAst ]

    prettyFailure inputAst err =
        T.unlines [ "Input AST:"
                  , Prettyprint.parserInverse prettyprinter inputAst
                  , "Parse error:"
                  , err ]
