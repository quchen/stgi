{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stg.Language.Prettyprint.ParserInverse (
    PrettyParserInverse(..),
) where



import qualified Data.Map                     as M
import qualified Data.Text                    as T
import           Prelude                      hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

import           Stg.Language



-- | Prettyprinter, defined as being compatible with the "Stg.Parser.Parser".
class PrettyParserInverse a where
    pprPI  ::  a  -> Doc
    pprsPI :: [a] -> Doc
    pprsPI = encloseSep "[" "]" "," . map pprPI
    {-# MINIMAL pprPI #-}

instance PrettyParserInverse Program where
    pprPI (Program binds) = pprPI binds

instance PrettyParserInverse Binds where
    pprPI (Binds bs) =
        (align . vsep . punctuate ";" . map prettyBinding . M.toList) bs
      where
        prettyBinding (var, lambda) =
            pprPI var <+> "=" <+> pprPI lambda

instance PrettyParserInverse LambdaForm where
    pprPI (LambdaForm free upd bound expr) =
        hsep [ pprsPI free
             , pprPI upd
             , pprsPI bound
             , "->"
             , pprPI expr ]

instance PrettyParserInverse UpdateFlag where
    pprPI = \case
        Update   -> "\\u"
        NoUpdate -> "\\n"

instance PrettyParserInverse Rec where
    pprPI = \case
        NonRecursive -> ""
        Recursive    -> "rec"

instance PrettyParserInverse Expr where
    pprPI = \case
        Let rec binds expr -> align (
            "let" <> pprPI rec <+> pprPI binds
            <$>
            "in" <+> pprPI expr )
        Case expr alts ->
            "case" <+> pprPI expr <+> "of"
            <$>
            indent 4 (pprPI alts)
        AppF var args -> pprPI var <+> pprsPI args
        AppC con args -> pprPI con <+> pprsPI args
        AppP op arg1 arg2 -> pprPI op <+> pprPI arg1 <+> pprPI arg2
        Lit lit -> pprPI lit

instance PrettyParserInverse Alts where
    pprPI = \case
        Algebraic alts -> pprPI alts
        Primitive alts -> pprPI alts

instance PrettyParserInverse AlgebraicAlts where
    pprPI (AlgebraicAlts alts def) =
        vsep (punctuate ";" (map (pprPI) alts ++ [pprPI def]))

instance PrettyParserInverse PrimitiveAlts where
    pprPI (PrimitiveAlts alts def) =
        vsep (punctuate ";" (map (pprPI) alts ++ [pprPI def]))

instance PrettyParserInverse AlgebraicAlt where
    pprPI (AlgebraicAlt con args expr) =
        pprPI con <+> pprsPI args <+> "->" <+> pprPI expr

instance PrettyParserInverse PrimitiveAlt where
    pprPI (PrimitiveAlt lit expr) =
        pprPI lit <+> "->" <+> pprPI expr

instance PrettyParserInverse DefaultAlt where
    pprPI = \case
        DefaultNotBound expr  -> "default" <+> "->" <+> pprPI expr
        DefaultBound var expr -> pprPI var <+> "->" <+> pprPI expr

instance PrettyParserInverse Literal where
    pprPI (Literal i) = int i <> "#"

instance PrettyParserInverse PrimOp where
    pprPI = \case
        Add -> "+#"
        Sub -> "-#"
        Mul -> "*#"
        Div -> "/#"
        Mod -> "%#"

instance PrettyParserInverse Var where
    pprPI (Var name) = string (T.unpack name)
    pprsPI = parens . align . hcat . punctuate "," . map pprPI

instance PrettyParserInverse Atom where
    pprPI = \case
        AtomVar var -> pprPI     var
        AtomLit lit -> pprPI lit
    pprsPI = parens . align . hcat . punctuate "," . map pprPI

instance PrettyParserInverse Constr where
    pprPI (Constr name) = string (T.unpack name)
