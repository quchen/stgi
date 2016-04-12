{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stg.Language.Prettyprint.ParserInverseAnsi (
    parserInverseAnsiModule,
) where



import qualified Data.Text                              as T
import           Prelude                                hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

import           Stg.Language
import           Stg.Language.Prettyprint.Module
import           Stg.Language.Prettyprint.ParserInverse



--------------------------------------------------------------------------------
-- Style definitions

-- | Keyword style
keywordC :: Doc -> Doc
keywordC = dullred

-- | Primitive style, for literals and functions
primC :: Doc -> Doc
primC = dullgreen

-- | Name style, for variables and constructors
nameC :: Doc -> Doc
nameC = dullyellow



-- | 'parserInverseModule', but with colours.
parserInverseAnsiModule :: PrettyprinterModule Doc
parserInverseAnsiModule = modifyModule parserInverseModule (\dict ->
    dict { pprExpr       = pprExpr'       dict
         , pprDefaultAlt = pprDefaultAlt' dict
         , pprLiteral    = pprLiteral'
         , pprPrimOp     = pprPrimOp'
         , pprVar        = pprVar'
         , pprConstr     = pprConstr'
         })

pprExpr' :: PrettyprinterDict Doc -> Expr -> Doc
pprExpr' dict = \case
    Let rec binds expr -> align (
        keywordC  "let" <> pprRec dict rec <+> pprBinds dict binds
        <$>
        keywordC "in" <+> pprExpr dict expr )
    Case expr alts ->
        keywordC "case" <+> pprExpr dict expr <+> keywordC "of"
        <$>
        indent 4 (pprAlts dict alts)
    AppF var args -> pprVar dict var <+> pprAtoms dict args
    AppC con args -> pprConstr dict con <+> pprAtoms dict args
    AppP op arg1 arg2 -> pprPrimOp dict op <+> pprAtom dict arg1 <+> pprAtom dict arg2
    Lit lit -> pprLiteral dict lit

pprDefaultAlt' :: PrettyprinterDict Doc -> DefaultAlt -> Doc
pprDefaultAlt' dict = \case
    DefaultNotBound expr  -> keywordC "default" <+> "->" <+> pprExpr dict expr
    DefaultBound var expr -> pprVar dict var    <+> "->" <+> pprExpr dict expr

pprLiteral' :: Literal -> Doc
pprLiteral' (Literal i) = primC (int i <> "#")

pprPrimOp' :: PrimOp -> Doc
pprPrimOp' op = primC (case op of
    Add -> "+#"
    Sub -> "-#"
    Mul -> "*#"
    Div -> "/#"
    Mod -> "%#" )

pprVar' :: Var -> Doc
pprVar' (Var name) = nameC (string (T.unpack name))

pprConstr' :: Constr -> Doc
pprConstr' (Constr name) = nameC (string (T.unpack name))
