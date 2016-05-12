{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Prettyprinting for ANSI terminals.
module Stg.Language.Prettyprint.Ansi (
    PrettyAnsi(..),
) where



import           Data.Foldable
import qualified Data.Map                     as M
import qualified Data.Text                    as T
import           Prelude                      hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

import Stg.Language



-- | Package of colour definitions used in this module.
data StgStateColours = StgStateColours
    { keyword :: Doc -> Doc
        -- ^ Keyword style
    , prim :: Doc -> Doc
        -- ^ Primitive style, for literals and functions
    , variable :: Doc -> Doc
        -- ^ Variable style
    , constructor :: Doc -> Doc
        -- ^ Constructor style
    , lambdaHead :: Doc -> Doc
        -- ^ Head of a lambda form
    , semicolon :: Doc -> Doc
        -- ^ Semicolons separating lists of bindings and alternatives
    }

-- | Colour definitions used in this module.
colour :: StgStateColours
colour = StgStateColours
    { keyword = id
    , prim = dullgreen
    , variable = dullyellow
    , constructor = dullmagenta
    , lambdaHead = id
    , semicolon = dullwhite
    }




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
    prettyAnsi :: a -> Doc
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
        (align . vsep . punctuate (semicolon colour ";") . map prettyBinding . M.assocs) bs
      where
        prettyBinding (var, lambda) =
            prettyAnsi var <+> "=" <+> prettyAnsi lambda

instance PrettyAnsi LambdaForm where
    prettyAnsi = prettyLambda (lambdaHead colour)
                              prettyAnsiList
                              prettyAnsi
                              prettyAnsiList
                              prettyAnsi

instance PrettyAnsi UpdateFlag

instance PrettyAnsi Rec where
    prettyAnsi = keyword colour . pretty

instance PrettyAnsi Expr where
    prettyAnsi = \case
        Let rec binds expr -> (align . vsep)
            [ keyword colour "let" <> prettyAnsi rec <+> (case rec of
                Recursive -> hardline <> indent 4 (prettyAnsi binds)
                NonRecursive -> prettyAnsi binds)
            , keyword colour "in" <+> prettyAnsi expr ]
        Case expr alts ->
            keyword colour "case" <+> prettyAnsi expr <+> keyword colour "of"
            <$>
            indent 4 (prettyAnsi alts)
        AppF var args -> prettyAnsi var <+> prettyAnsiList args
        AppC con args -> prettyAnsi con <+> prettyAnsiList args
        AppP op arg1 arg2 -> prettyAnsi op <+> prettyAnsi arg1 <+> prettyAnsi arg2
        Lit lit -> prettyAnsi lit

instance PrettyAnsi Alts where
    prettyAnsi (Alts NoNonDefaultAlts def) = prettyAnsi def
    prettyAnsi (Alts (AlgebraicAlts alts) def) =
        (align . vsep . punctuate (semicolon colour ";"))
            (map prettyAnsi (toList alts) ++ [prettyAnsi def])
    prettyAnsi (Alts (PrimitiveAlts alts) def) =
        (align . vsep . punctuate (semicolon colour ";"))
            (map prettyAnsi (toList alts) ++ [prettyAnsi def])

instance PrettyAnsi AlgebraicAlt where
    prettyAnsi (AlgebraicAlt con args expr) =
        prettyAnsi con <+> prettyAnsiList args <+> "->" <+> prettyAnsi expr

instance PrettyAnsi PrimitiveAlt where
    prettyAnsi (PrimitiveAlt lit expr) =
        prettyAnsi lit <+> "->" <+> prettyAnsi expr

instance PrettyAnsi DefaultAlt where
    prettyAnsi = \case
        DefaultNotBound expr  -> keyword colour "default" <+> "->" <+> prettyAnsi expr
        DefaultBound var expr -> prettyAnsi var <+> "->" <+> prettyAnsi expr

instance PrettyAnsi Literal where
    prettyAnsi (Literal i) = prim colour (integer i <> "#")

instance PrettyAnsi PrimOp where
    prettyAnsi = prim colour . pretty

instance PrettyAnsi Var where
    prettyAnsi (Var var) = variable colour (string (T.unpack var))
    prettyAnsiList = parens . align . hcat . punctuate "," . map prettyAnsi

instance PrettyAnsi Atom where
    prettyAnsi = \case
        AtomVar var -> prettyAnsi var
        AtomLit lit -> prettyAnsi lit
    prettyAnsiList = parens . align . hcat . punctuate "," . map prettyAnsi

instance PrettyAnsi Constr where
    prettyAnsi (Constr con) = constructor colour (string (T.unpack con))
