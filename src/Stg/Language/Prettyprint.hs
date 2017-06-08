{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Prettyprinting STG elements in various formats.
module Stg.Language.Prettyprint (
    PrettyStgi(..),
    StgiAnn(..),
    StateAnn(..),
    AstAnn(..),

    renderRich,
    renderPlain,
    prettyprintOldAnsi,
) where



import           Data.Text                                           (Text)
import qualified Data.Text                                           as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal           as PrettyAnsi
import           Data.Text.Prettyprint.Doc.Render.Text               as PrettyPlain
import           Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree
import           Prelude                                             hiding
    ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen                        as Leijen



renderRich :: Doc StgiAnn -> Text
renderRich = PrettyAnsi.renderStrict . alterAnnotationsS terminalStyle . layoutPretty layoutOptions
  where
    terminalStyle :: StgiAnn -> Maybe AnsiTerminal
    terminalStyle = \case
        StateAnn x -> case x of
            Headline       -> Just (Color Foreground Dull Blue)
            Address        -> Just (Color Foreground Dull Cyan)
            AddressCore    -> Just Underlined
            ClosureType    -> Just Bold
            StackFrameType -> Just Bold
        AstAnn x -> case x of
            Keyword     -> Nothing
            Prim        -> Just (Color Foreground Dull Green)
            Variable    -> Just (Color Foreground Dull Yellow)
            Constructor -> Just (Color Foreground Dull Magenta)
            Semicolon   -> Just (Color Foreground Dull White)

renderPlain :: Doc ann -> Text
renderPlain = PrettyPlain.renderStrict . layoutPretty layoutOptions

layoutOptions :: LayoutOptions
layoutOptions = defaultLayoutOptions { layoutPageWidth = Unbounded }

-- | Prettyprint a value as 'Text', including styles such as colours.
prettyprintOldAnsi :: Leijen.Doc -> Text
prettyprintOldAnsi input = T.pack (Leijen.displayS (Leijen.renderPretty 0.4 1000 input) "")

class PrettyStgi a where
    prettyStgi :: a -> Doc StgiAnn

data StgiAnn
    = StateAnn StateAnn
    | AstAnn AstAnn

-- | Semantic annotations for rendering.
data StateAnn
    = Headline
        -- ^ Style of headlines in the state overview, such as \"Heap" and
        --   "Frame i".

    | Address
        -- ^ Style of memory addresses, including @0x@ prefix.

    | AddressCore
        -- ^ Style of memory addresses; applied only to the actual address
        --   number, such as @ff@ in @0xff@.

    | ClosureType
        -- ^ Style of the type of a closure, such as BLACKHOLE or FUN.

    | StackFrameType
        -- ^ Style of the stack frame annotation, such as UPD or ARG.

-- | The different semantic annotations an STG AST element can have.
data AstAnn
    = Keyword
    | Prim
    | Variable
    | Constructor
    | Semicolon

instance PrettyStgi Bool where prettyStgi = pretty
instance PrettyStgi Int where prettyStgi = pretty
instance PrettyStgi Integer where prettyStgi = pretty
instance (PrettyStgi a, PrettyStgi b) => PrettyStgi (a,b) where prettyStgi (a,b) = tupled [prettyStgi a, prettyStgi b]
instance PrettyStgi a => PrettyStgi [a] where prettyStgi = list . map prettyStgi
