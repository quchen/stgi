{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stg.Language.Prettyprint.ParserInverseAnsi (
    parserInverseColouredModule
) where



import qualified Data.Text                              as T
import           Prelude                                hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

import           Stg.Language
import           Stg.Language.Prettyprint.Module
import           Stg.Language.Prettyprint.ParserInverse


--------------------------------------------------------------------------------
-- Style definitions

keywordC :: Doc -> Doc
keywordC = red

primC :: Doc -> Doc
primC = green

nameC :: Doc -> Doc
nameC = yellow



-- | 'parserInverseModule', but with colours.
parserInverseColouredModule :: PrettyprinterModule Doc
parserInverseColouredModule = modifyModule parserInverseModule (\m ->
    m { pprExpr       = pprExpr'       m
      , pprDefaultAlt = pprDefaultAlt' m
      , pprLiteral    = pprLiteral'    m
      , pprPrimOp     = pprPrimOp'     m
      , pprVar        = pprVar'        m
      , pprConstr     = pprConstr'     m
      })

pprExpr' :: PrettyprinterDict Doc -> Expr -> Doc
pprExpr' ppr = \case
    Let rec binds expr -> align (
        keywordC  "let" <> pprRec ppr rec <+> pprBinds ppr binds
        <$>
        keywordC "in" <+> pprExpr ppr expr )
    Case expr alts ->
        keywordC "case" <+> pprExpr ppr expr <+> keywordC "of"
        <$>
        indent 4 (pprAlts ppr alts)
    AppF var args -> pprVar ppr var <+> pprAtoms ppr args
    AppC con args -> pprConstr ppr con <+> pprAtoms ppr args
    AppP op arg1 arg2 -> pprPrimOp ppr op <+> pprAtom ppr arg1 <+> pprAtom ppr arg2
    Lit lit -> pprLiteral ppr lit

pprDefaultAlt' :: PrettyprinterDict Doc -> DefaultAlt -> Doc
pprDefaultAlt' ppr = \case
    DefaultNotBound body  -> keywordC "default" <+> "->" <+> pprExpr ppr body
    DefaultBound var body -> pprVar ppr var <+> "->" <+> pprExpr ppr body

pprLiteral' :: ppr -> Literal -> Doc
pprLiteral' _ppr (Literal i) = primC (int i <> "#")

pprPrimOp' :: ppr -> PrimOp -> Doc
pprPrimOp' _ppr op = primC (case op of
    Add -> "+#"
    Sub -> "-#"
    Mul -> "*#"
    Div -> "/#"
    Mod -> "%#" )

pprVar' :: ppr -> Var -> Doc
pprVar' _ppr (Var name) = nameC (string (T.unpack name))

pprConstr' :: ppr -> Constr -> Doc
pprConstr' _ppr (Constr name) = nameC (string (T.unpack name))
