{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Prettyprinting for ANSI terminals.
module Stg.Language.Prettyprint.Ansi (
    PrettyAnsi(..),
) where



import qualified Data.Map                     as M
import qualified Data.Text                    as T
import           Prelude                      hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

import           Stg.Language



-- | Keyword style
keyword :: Doc -> Doc
keyword = dullblue

-- | Primitive style, for literals and functions
prim :: Doc -> Doc
prim = dullgreen

-- | Name style, for variables and constructors
name :: Doc -> Doc
name = dullyellow

-- | Head of a lambda form
lambdaHead :: Doc -> Doc
lambdaHead = dullcyan

-- | Semicolons separating lists of bindings and alternatives
semicolon :: Doc -> Doc
semicolon = dullwhite



-- | Prettyprinter, defined as being compatible with the "Stg.Parser.Parser",
-- but with ANSI terminal syntax highlighting.
--
-- The 'plain'ed version of 'ppr' should match the uncoloured one.
--
-- @
-- 'plain' . 'prettyAnsi'     ≡ 'pretty'
-- 'plain' . 'prettyAnsiList' ≡ 'prettyList'
-- @
class Pretty a => PrettyAnsi a where
    prettyAnsi  :: a -> Doc
    prettyAnsi = pretty
    prettyAnsiList :: [a] -> Doc
    prettyAnsiList = prettyList

instance PrettyAnsi Doc

instance PrettyAnsi Integer where
    prettyAnsi = text . show

instance PrettyAnsi () where
    prettyAnsi = text . show

instance PrettyAnsi Char

instance (PrettyAnsi a, PrettyAnsi b) => PrettyAnsi (a,b) where
    prettyAnsi (x,y) = (parens . vsep . punctuate comma)
        [prettyAnsi x, prettyAnsi y]

instance (PrettyAnsi a, PrettyAnsi b, PrettyAnsi c) => PrettyAnsi (a,b,c) where
    prettyAnsi (x,y,z) = (parens . vsep . punctuate comma)
        [prettyAnsi x, prettyAnsi y, prettyAnsi z]

instance PrettyAnsi Program where
    prettyAnsi (Program binds) = prettyAnsi binds

instance PrettyAnsi Binds where
    prettyAnsi (Binds bs) =
        (align . vsep . punctuate (semicolon ";") . map prettyBinding . M.toList) bs
      where
        prettyBinding (var, lambda) =
            prettyAnsi var <+> "=" <+> prettyAnsi lambda

instance PrettyAnsi LambdaForm where
    prettyAnsi = prettyLambda lambdaHead
                              prettyAnsiList
                              prettyAnsi
                              prettyAnsiList
                              prettyAnsi

instance PrettyAnsi UpdateFlag

instance PrettyAnsi Rec

instance PrettyAnsi Expr where
    prettyAnsi = \case
        Let rec binds expr -> align (
            keyword "let" <> prettyAnsi rec <+> prettyAnsi binds
            <$>
            keyword "in" <+> prettyAnsi expr )
        Case expr alts ->
            keyword "case" <+> prettyAnsi expr <+> keyword "of"
            <$>
            indent 4 (prettyAnsi alts)
        AppF var args -> prettyAnsi var <+> prettyAnsiList args
        AppC con args -> prettyAnsi con <+> prettyAnsiList args
        AppP op arg1 arg2 -> prettyAnsi op <+> prettyAnsi arg1 <+> prettyAnsi arg2
        Lit lit -> prettyAnsi lit

instance PrettyAnsi Alts where
    prettyAnsi = \case
        Algebraic alts -> prettyAnsi alts
        Primitive alts -> prettyAnsi alts

instance PrettyAnsi AlgebraicAlts where
    prettyAnsi (AlgebraicAlts alts def) =
        vsep (punctuate (semicolon ";") (map prettyAnsi alts ++ [prettyAnsi def]))

instance PrettyAnsi PrimitiveAlts where
    prettyAnsi (PrimitiveAlts alts def) =
        vsep (punctuate (semicolon ";") (map prettyAnsi alts ++ [prettyAnsi def]))

instance PrettyAnsi AlgebraicAlt where
    prettyAnsi (AlgebraicAlt con args expr) =
        prettyAnsi con <+> prettyAnsiList args <+> "->" <+> prettyAnsi expr

instance PrettyAnsi PrimitiveAlt where
    prettyAnsi (PrimitiveAlt lit expr) =
        prettyAnsi lit <+> "->" <+> prettyAnsi expr

instance PrettyAnsi DefaultAlt where
    prettyAnsi = \case
        DefaultNotBound expr  -> keyword "default" <+> "->" <+> prettyAnsi expr
        DefaultBound var expr -> prettyAnsi var <+> "->" <+> prettyAnsi expr

instance PrettyAnsi Literal where
    prettyAnsi (Literal i) = prim (integer i <> "#")

instance PrettyAnsi PrimOp where
    prettyAnsi op = prim (case op of
        Add -> "+#"
        Sub -> "-#"
        Mul -> "*#"
        Div -> "/#"
        Mod -> "%#" )

instance PrettyAnsi Var where
    prettyAnsi (Var var) = name (string (T.unpack var))
    prettyAnsiList = parens . align . hcat . punctuate "," . map prettyAnsi

instance PrettyAnsi Atom where
    prettyAnsi = \case
        AtomVar var -> prettyAnsi     var
        AtomLit lit -> prettyAnsi lit
    prettyAnsiList = parens . align . hcat . punctuate "," . map prettyAnsi

instance PrettyAnsi Constr where
    prettyAnsi (Constr con) = name (string (T.unpack con))
