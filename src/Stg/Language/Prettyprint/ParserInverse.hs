{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stg.Language.Prettyprint.ParserInverse (
    PrettyParserInverse(..),
) where



import           Text.PrettyPrint.ANSI.Leijen

import           Stg.Language



-- | Prettyprinter, defined as being compatible with the "Stg.Parser.Parser".
class Pretty a => PrettyParserInverse a where
    pprPI ::  a  -> Doc
    pprPI = pretty
    pprsPI :: [a] -> Doc
    pprsPI = prettyList
    {-# MINIMAL #-}

instance PrettyParserInverse Program
instance PrettyParserInverse Binds
instance PrettyParserInverse LambdaForm
instance PrettyParserInverse UpdateFlag
instance PrettyParserInverse Rec
instance PrettyParserInverse Expr
instance PrettyParserInverse Alts
instance PrettyParserInverse AlgebraicAlts
instance PrettyParserInverse PrimitiveAlts
instance PrettyParserInverse AlgebraicAlt
instance PrettyParserInverse PrimitiveAlt
instance PrettyParserInverse DefaultAlt
instance PrettyParserInverse Literal
instance PrettyParserInverse PrimOp
instance PrettyParserInverse Var
instance PrettyParserInverse Atom
instance PrettyParserInverse Constr
