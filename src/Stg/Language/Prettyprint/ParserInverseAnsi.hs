{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stg.Language.Prettyprint.ParserInverseAnsi (
    PrettyParserInverseAnsi(..),
) where



import qualified Data.Map                     as M
import qualified Data.Text                    as T
import           Prelude                      hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

import           Stg.Language



-- | Keyword style
keyword :: Doc -> Doc
keyword = dullred

-- | Primitive style, for literals and functions
prim :: Doc -> Doc
prim = dullgreen

-- | Name style, for variables and constructors
name :: Doc -> Doc
name = dullyellow

-- | Head of a lambda form
lambdaHead :: Doc -> Doc
lambdaHead = dullblue

-- | Semicolons separating lists of bindings and alternatives
semicolon :: Doc -> Doc
semicolon = dullwhite



-- | Prettyprinter, defined as being compatible with the "Stg.Parser.Parser",
-- but with ANSI terminal syntax highlighting.
--
-- The 'plain'ed version of 'ppr' should match the uncoloured one.
--
-- @
-- 'plain' . 'pprAnsi'  ≡ 'pretty'
-- 'plain' . 'pprsAnsi' ≡ 'prettyList'
-- @
class Pretty a => PrettyParserInverseAnsi a where
    pprAnsi  :: a -> Doc
    pprAnsi = pretty
    pprsAnsi :: [a] -> Doc
    pprsAnsi = prettyList



instance PrettyParserInverseAnsi Program where
    pprAnsi (Program binds) = pprAnsi binds

instance PrettyParserInverseAnsi Binds where
    pprAnsi (Binds bs) =
        (align . vsep . punctuate (semicolon ";") . map prettyBinding . M.toList) bs
      where
        prettyBinding (var, lambda) =
            pprAnsi var <+> "=" <+> pprAnsi lambda

instance PrettyParserInverseAnsi LambdaForm where
    pprAnsi (LambdaForm free upd bound expr) =
        lambdaHead (pprsAnsi free
             <+> pprAnsi upd
             <+> pprsAnsi bound)
        <+> "->"
        <+> pprAnsi expr

instance PrettyParserInverseAnsi UpdateFlag

instance PrettyParserInverseAnsi Rec

instance PrettyParserInverseAnsi Expr where
    pprAnsi = \case
        Let rec binds expr -> align (
            keyword "let" <> pprAnsi rec <+> pprAnsi binds
            <$>
            keyword "in" <+> pprAnsi expr )
        Case expr alts ->
            keyword "case" <+> pprAnsi expr <+> keyword "of"
            <$>
            indent 4 (pprAnsi alts)
        AppF var args -> pprAnsi var <+> pprsAnsi args
        AppC con args -> pprAnsi con <+> pprsAnsi args
        AppP op arg1 arg2 -> pprAnsi op <+> pprAnsi arg1 <+> pprAnsi arg2
        Lit lit -> pprAnsi lit

instance PrettyParserInverseAnsi Alts where
    pprAnsi = \case
        Algebraic alts -> pprAnsi alts
        Primitive alts -> pprAnsi alts

instance PrettyParserInverseAnsi AlgebraicAlts where
    pprAnsi (AlgebraicAlts alts def) =
        vsep (punctuate (semicolon ";") (map pprAnsi alts ++ [pprAnsi def]))

instance PrettyParserInverseAnsi PrimitiveAlts where
    pprAnsi (PrimitiveAlts alts def) =
        vsep (punctuate (semicolon ";") (map pprAnsi alts ++ [pprAnsi def]))

instance PrettyParserInverseAnsi AlgebraicAlt where
    pprAnsi (AlgebraicAlt con args expr) =
        pprAnsi con <+> pprsAnsi args <+> "->" <+> pprAnsi expr

instance PrettyParserInverseAnsi PrimitiveAlt where
    pprAnsi (PrimitiveAlt lit expr) =
        pprAnsi lit <+> "->" <+> pprAnsi expr

instance PrettyParserInverseAnsi DefaultAlt where
    pprAnsi = \case
        DefaultNotBound expr  -> keyword "default" <+> "->" <+> pprAnsi expr
        DefaultBound var expr -> pprAnsi var <+> "->" <+> pprAnsi expr

instance PrettyParserInverseAnsi Literal where
    pprAnsi (Literal i) = prim (int i <> "#")

instance PrettyParserInverseAnsi PrimOp where
    pprAnsi op = prim (case op of
        Add -> "+#"
        Sub -> "-#"
        Mul -> "*#"
        Div -> "/#"
        Mod -> "%#" )

instance PrettyParserInverseAnsi Var where
    pprAnsi (Var var) = name (string (T.unpack var))
    pprsAnsi = parens . align . hcat . punctuate "," . map pprAnsi

instance PrettyParserInverseAnsi Atom where
    pprAnsi = \case
        AtomVar var -> pprAnsi     var
        AtomLit lit -> pprAnsi lit
    pprsAnsi = parens . align . hcat . punctuate "," . map pprAnsi

instance PrettyParserInverseAnsi Constr where
    pprAnsi (Constr con) = name (string (T.unpack con))
