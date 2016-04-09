{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stg.Language.Prettyprint.ParserInverse (
    parserInverseModule
) where



import qualified Data.Map                        as M
import qualified Data.Text                       as T
import           Prelude                         hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

import           Stg.Language
import           Stg.Language.Prettyprint.Module



-- | Prettyprinter, defined as being compatible with the "Stg.Parser.Parser".
parserInverseModule :: PrettyprinterModule Doc
parserInverseModule = PrettyprinterModule (\rec -> PrettyprinterDict
    { pprProgram       = pprProgram'       rec
    , pprBinds         = pprBinds'         rec
    , pprLambdaForm    = pprLambdaForm'    rec
    , pprUpdateFlag    = pprUpdateFlag'    rec
    , pprRec           = pprRec'           rec
    , pprExpr          = pprExpr'          rec
    , pprAlts          = pprAlts'          rec
    , pprAlgebraicAlts = pprAlgebraicAlts' rec
    , pprPrimitiveAlts = pprPrimitiveAlts' rec
    , pprAlgebraicAlt  = pprAlgebraicAlt'  rec
    , pprPrimitiveAlt  = pprPrimitiveAlt'  rec
    , pprDefaultAlt    = pprDefaultAlt'    rec
    , pprLiteral       = pprLiteral'       rec
    , pprPrimOp        = pprPrimOp'        rec
    , pprVar           = pprVar'           rec
    , pprVars          = pprVars'          rec
    , pprAtom          = pprAtom'          rec
    , pprAtoms         = pprAtoms'         rec
    , pprConstr        = pprConstr'        rec
    })

pprProgram' :: PrettyprinterDict Doc -> Program -> Doc
pprProgram' ppr (Program binds) = pprBinds ppr binds

pprBinds' :: PrettyprinterDict Doc -> Binds -> Doc
pprBinds' ppr (Binds bs) =
    (align . vsep . punctuate ";" . map prettyBinding . M.toList) bs
  where
    prettyBinding (var, lambda) =
        pprVar ppr var <+> "=" <+> pprLambdaForm ppr lambda

pprLambdaForm' :: PrettyprinterDict Doc -> LambdaForm -> Doc
pprLambdaForm' ppr (LambdaForm free upd bound body) =
        hsep [ pprVars ppr free
             , pprUpdateFlag ppr upd
             , pprVars ppr bound
             , "->"
             , pprExpr ppr body ]

pprUpdateFlag' :: ppr -> UpdateFlag -> Doc
pprUpdateFlag' _ppr = \case
    Update   -> "\\u"
    NoUpdate -> "\\n"

pprRec' :: ppr -> Rec -> Doc
pprRec' _ppr = \case
    NonRecursive -> ""
    Recursive    -> "rec"

pprExpr' :: PrettyprinterDict Doc -> Expr -> Doc
pprExpr' ppr = \case
    Let rec binds expr -> align (
        "let" <> pprRec ppr rec <+> pprBinds ppr binds
        <$>
        "in" <+> pprExpr ppr expr )
    Case expr alts ->
        "case" <+> pprExpr ppr expr <+> "of"
        <$>
        indent 4 (pprAlts ppr alts)
    AppF var args -> pprVar ppr var <+> pprAtoms ppr args
    AppC con args -> pprConstr ppr con <+> pprAtoms ppr args
    AppP op arg1 arg2 -> pprPrimOp ppr op <+> pprAtom ppr arg1 <+> pprAtom ppr arg2
    Lit lit -> pprLiteral ppr lit

pprAlts' :: PrettyprinterDict Doc -> Alts -> Doc
pprAlts' ppr = \case
    Algebraic alts -> pprAlgebraicAlts ppr alts
    Primitive alts -> pprPrimitiveAlts ppr alts

pprAlgebraicAlts' :: PrettyprinterDict Doc -> AlgebraicAlts -> Doc
pprAlgebraicAlts' ppr (AlgebraicAlts alts def) =
    vsep (punctuate ";" (map (pprAlgebraicAlt ppr) alts ++ [pprDefaultAlt ppr def]))

pprPrimitiveAlts' :: PrettyprinterDict Doc -> PrimitiveAlts -> Doc
pprPrimitiveAlts' ppr (PrimitiveAlts alts def) =
    vsep (punctuate ";" (map (pprPrimitiveAlt ppr) alts ++ [pprDefaultAlt ppr def]))

pprAlgebraicAlt' :: PrettyprinterDict Doc -> AlgebraicAlt -> Doc
pprAlgebraicAlt' ppr (AlgebraicAlt con args body) =
    pprConstr ppr con <+> pprVars ppr args <+> "->" <+> pprExpr ppr body

pprPrimitiveAlt' :: PrettyprinterDict Doc -> PrimitiveAlt -> Doc
pprPrimitiveAlt' ppr (PrimitiveAlt lit body) =
    pprLiteral ppr lit <+> "->" <+> pprExpr ppr body

pprDefaultAlt' :: PrettyprinterDict Doc -> DefaultAlt -> Doc
pprDefaultAlt' ppr = \case
    DefaultNotBound body  -> "default" <+> "->" <+> pprExpr ppr body
    DefaultBound var body -> pprVar ppr var <+> "->" <+> pprExpr ppr body

pprLiteral' :: ppr -> Literal -> Doc
pprLiteral' _ppr (Literal i) = int i <> "#"

pprPrimOp' :: ppr -> PrimOp -> Doc
pprPrimOp' _ppr = \case
    Add -> "+#"
    Sub -> "-#"
    Mul -> "*#"
    Div -> "/#"
    Mod -> "%#"

pprVar' :: ppr -> Var -> Doc
pprVar' _ppr (Var name) = string (T.unpack name)

pprVars' :: PrettyprinterDict Doc -> [Var] -> Doc
pprVars' ppr  = parens . align . hcat . punctuate ", " . map (pprVar ppr)

pprAtom' :: PrettyprinterDict Doc -> Atom -> Doc
pprAtom' ppr = \case
    AtomVar var -> pprVar     ppr var
    AtomLit lit -> pprLiteral ppr lit

pprAtoms' :: PrettyprinterDict Doc -> [Atom] -> Doc
pprAtoms' ppr = parens . align . hcat . punctuate ", " . map (pprAtom ppr)

pprConstr' :: ppr -> Constr -> Doc
pprConstr' _ppr (Constr name) = string (T.unpack name)
