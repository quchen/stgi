{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Stg.Language.Prettyprint (prettyprint) where



import qualified Data.Map                     as M
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Prelude                      hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

import           Stg.Language

prettyprint :: Pretty a => a -> Text
prettyprint x = T.pack (displayS (renderPretty 0.4 80 (pretty x)) "")

instance Pretty Program where
    pretty (Program binds) = pretty binds

instance Pretty Binds where
    pretty (Binds bs) = (align . vsep . punctuate ";" . map prettyBinding . M.toList) bs
      where
        prettyBinding (var, lambda) = pretty var <+> "=" <+> pretty lambda

instance Pretty LambdaForm where
    pretty (LambdaForm free upd bound body) =
        hsep [ prettyVars free
             , pretty upd
             , prettyVars bound
             , "->"
             , pretty body ]

-- | [a,b,c] ==> "(a, b, c)"
parensCommaSep :: Pretty a => [a] -> Doc
parensCommaSep = parens . align . hcat . punctuate ", " . map pretty

prettyVars :: [Var] -> Doc
prettyVars = parensCommaSep

prettyAtoms :: [Atom] -> Doc
prettyAtoms = parensCommaSep

instance Pretty UpdateFlag where
    pretty Update   = "\\u"
    pretty NoUpdate = "\\n"

instance Pretty Rec where
    pretty NonRecursive = ""
    pretty Recursive    = "rec"

instance Pretty Expr where
    pretty (Let rec binds expr) = align (
        "let" <> pretty rec <+> pretty binds
        <$>
        "in" <+> pretty expr )
    pretty (Case expr alts) =
        "case" <+> pretty expr <+> "of"
        <$>
        indent 4 (pretty alts)
    pretty (AppF var args) = pretty var <+> prettyAtoms args
    pretty (AppC con args) = pretty con <+> prettyAtoms args
    pretty (AppP op arg1 arg2) = pretty op <+> pretty arg1 <+> pretty arg2
    pretty (Lit lit) = pretty lit

instance Pretty Alts where
    pretty (Algebraic alts) = pretty alts
    pretty (Primitive alts) = pretty alts

prettyAlts :: (Pretty a, Pretty b) => [a] -> b -> Doc
prettyAlts alts def = vsep (punctuate ";" (map pretty alts ++ [pretty def]))

instance Pretty AlgebraicAlts where
    pretty (AlgebraicAlts alts def) = prettyAlts alts def

instance Pretty PrimitiveAlts where
    pretty (PrimitiveAlts alts def) = prettyAlts alts def

instance Pretty AlgebraicAlt where
    pretty (AlgebraicAlt con args body) =
        pretty con <+> prettyVars args <+> "->" <+> pretty body

instance Pretty PrimitiveAlt where
    pretty (PrimitiveAlt lit body) =
        pretty lit <+> "->" <+> pretty body

instance Pretty DefaultAlt where
    pretty (DefaultNotBound body) = "default" <+> "->" <+> pretty body
    pretty (DefaultBound var body) = pretty var <+> "->" <+> pretty body

instance Pretty Literal where
    pretty (Literal i) = pretty i <> "#"

instance Pretty PrimOp where
    pretty Add = "+#"
    pretty Sub = "-#"
    pretty Mul = "*#"
    pretty Div = "/#"
    pretty Mod = "%#"

instance Pretty Var where
    pretty (Var name) = pretty (T.unpack name)

instance Pretty Atom where
    pretty (AtomVar var) = pretty var
    pretty (AtomLit lit) = pretty lit

instance Pretty Constr where
    pretty (Constr name) = pretty (T.unpack name)
