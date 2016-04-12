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
parserInverseModule = makeModule (\dict -> PrettyprinterDict
    { pprProgram       = pprProgram'       dict
    , pprBinds         = pprBinds'         dict
    , pprLambdaForm    = pprLambdaForm'    dict
    , pprUpdateFlag    = pprUpdateFlag'
    , pprRec           = pprRec'
    , pprExpr          = pprExpr'          dict
    , pprAlts          = pprAlts'          dict
    , pprAlgebraicAlts = pprAlgebraicAlts' dict
    , pprPrimitiveAlts = pprPrimitiveAlts' dict
    , pprAlgebraicAlt  = pprAlgebraicAlt'  dict
    , pprPrimitiveAlt  = pprPrimitiveAlt'  dict
    , pprDefaultAlt    = pprDefaultAlt'    dict
    , pprLiteral       = pprLiteral'
    , pprPrimOp        = pprPrimOp'
    , pprVar           = pprVar'
    , pprVars          = pprVars'          dict
    , pprAtom          = pprAtom'          dict
    , pprAtoms         = pprAtoms'         dict
    , pprConstr        = pprConstr'
    })

pprProgram' :: PrettyprinterDict Doc -> Program -> Doc
pprProgram' dict (Program binds) = pprBinds dict binds

pprBinds' :: PrettyprinterDict Doc -> Binds -> Doc
pprBinds' dict (Binds bs) =
    (align . vsep . punctuate ";" . map prettyBinding . M.toList) bs
  where
    prettyBinding (var, lambda) =
        pprVar dict var <+> "=" <+> pprLambdaForm dict lambda

pprLambdaForm' :: PrettyprinterDict Doc -> LambdaForm -> Doc
pprLambdaForm' dict (LambdaForm free upd bound expr) =
        hsep [ pprVars dict free
             , pprUpdateFlag dict upd
             , pprVars dict bound
             , "->"
             , pprExpr dict expr ]

pprUpdateFlag' :: UpdateFlag -> Doc
pprUpdateFlag' = \case
    Update   -> "\\u"
    NoUpdate -> "\\n"

pprRec' :: Rec -> Doc
pprRec' = \case
    NonRecursive -> ""
    Recursive    -> "rec"

pprExpr' :: PrettyprinterDict Doc -> Expr -> Doc
pprExpr' dict = \case
    Let rec binds expr -> align (
        "let" <> pprRec dict rec <+> pprBinds dict binds
        <$>
        "in" <+> pprExpr dict expr )
    Case expr alts ->
        "case" <+> pprExpr dict expr <+> "of"
        <$>
        indent 4 (pprAlts dict alts)
    AppF var args -> pprVar dict var <+> pprAtoms dict args
    AppC con args -> pprConstr dict con <+> pprAtoms dict args
    AppP op arg1 arg2 -> pprPrimOp dict op <+> pprAtom dict arg1 <+> pprAtom dict arg2
    Lit lit -> pprLiteral dict lit

pprAlts' :: PrettyprinterDict Doc -> Alts -> Doc
pprAlts' dict = \case
    Algebraic alts -> pprAlgebraicAlts dict alts
    Primitive alts -> pprPrimitiveAlts dict alts

pprAlgebraicAlts' :: PrettyprinterDict Doc -> AlgebraicAlts -> Doc
pprAlgebraicAlts' dict (AlgebraicAlts alts def) =
    vsep (punctuate ";" (map (pprAlgebraicAlt dict) alts ++ [pprDefaultAlt dict def]))

pprPrimitiveAlts' :: PrettyprinterDict Doc -> PrimitiveAlts -> Doc
pprPrimitiveAlts' dict (PrimitiveAlts alts def) =
    vsep (punctuate ";" (map (pprPrimitiveAlt dict) alts ++ [pprDefaultAlt dict def]))

pprAlgebraicAlt' :: PrettyprinterDict Doc -> AlgebraicAlt -> Doc
pprAlgebraicAlt' dict (AlgebraicAlt con args expr) =
    pprConstr dict con <+> pprVars dict args <+> "->" <+> pprExpr dict expr

pprPrimitiveAlt' :: PrettyprinterDict Doc -> PrimitiveAlt -> Doc
pprPrimitiveAlt' dict (PrimitiveAlt lit expr) =
    pprLiteral dict lit <+> "->" <+> pprExpr dict expr

pprDefaultAlt' :: PrettyprinterDict Doc -> DefaultAlt -> Doc
pprDefaultAlt' dict = \case
    DefaultNotBound expr  -> "default" <+> "->" <+> pprExpr dict expr
    DefaultBound var expr -> pprVar dict var <+> "->" <+> pprExpr dict expr

pprLiteral' :: Literal -> Doc
pprLiteral' (Literal i) = int i <> "#"

pprPrimOp' :: PrimOp -> Doc
pprPrimOp' = \case
    Add -> "+#"
    Sub -> "-#"
    Mul -> "*#"
    Div -> "/#"
    Mod -> "%#"

pprVar' :: Var -> Doc
pprVar' (Var name) = string (T.unpack name)

pprVars' :: PrettyprinterDict Doc -> [Var] -> Doc
pprVars' dict  = parens . align . hcat . punctuate "," . map (pprVar dict)

pprAtom' :: PrettyprinterDict Doc -> Atom -> Doc
pprAtom' dict = \case
    AtomVar var -> pprVar     dict var
    AtomLit lit -> pprLiteral dict lit

pprAtoms' :: PrettyprinterDict Doc -> [Atom] -> Doc
pprAtoms' dict = parens . align . hcat . punctuate "," . map (pprAtom dict)

pprConstr' :: Constr -> Doc
pprConstr' (Constr name) = string (T.unpack name)
