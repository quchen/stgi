{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as LTB

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree

import qualified Stg.ExamplePrograms as ExampleProgram



import Stg.Language.Prettyprint
    (AstAnn (..), PrettyStgi (..), StateAnn (..), StgiAnn (..))


main :: IO ()
main = (T.putStrLn . renderReactHtml . prettyStgi) ExampleProgram.fibonacciZipWith


renderReactHtml :: Doc StgiAnn -> Text
renderReactHtml
    = LT.toStrict
    . LTB.toLazyText
    . toRawHtmlBuilder
    . alterAnnotationsST reactStyle
    . treeForm
    . layoutPretty layoutOptions
  where

    reactStyle :: StgiAnn -> [HtmlClass]
    reactStyle = \case
        StateAnn Headline       -> [HtmlClass "headline"]
        StateAnn Address        -> [HtmlClass "address"]
        StateAnn AddressCore    -> [HtmlClass "addresscore"]
        StateAnn ClosureType    -> [HtmlClass "closuretype"]
        StateAnn StackFrameType -> [HtmlClass "stackframetype"]

        AstAnn Keyword     -> [HtmlClass "keyword"]
        AstAnn Prim        -> [HtmlClass "prim"]
        AstAnn Variable    -> [HtmlClass "variable"]
        AstAnn Constructor -> [HtmlClass "constructor"]
        AstAnn Semicolon   -> [HtmlClass "semicolon"]

    toRawHtmlBuilder :: SimpleDocTree HtmlClass -> LTB.Builder
    toRawHtmlBuilder = \case
        STEmpty -> mempty
        STChar c -> LTB.singleton c
        STText _ t -> LTB.fromText t
        STLine i -> "<br>\n" <> LTB.fromText (T.replicate i " ")
        STAnn (HtmlClass c) x -> "<span class=\"" <> LTB.fromText c <> "\">"
                              <> toRawHtmlBuilder x
                              <> "</span>"
        STConcat xs -> mconcat (map toRawHtmlBuilder xs)

    layoutOptions = defaultLayoutOptions { layoutPageWidth = Unbounded }

newtype HtmlClass = HtmlClass Text
    deriving (Eq, Ord, Show)
