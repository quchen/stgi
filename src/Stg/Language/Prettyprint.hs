{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Prettyprinting STG elements in various formats.
module Stg.Language.Prettyprint (
    Pretty(..),
    prettyprint,
    prettyprintPlain,
) where



import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Prelude                      hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen



-- | Prettyprint a value as 'Text', including styles such as colours.
prettyprint :: Pretty a => a -> Text
prettyprint = prettyprintModified id

-- | Prettyprint a value as 'Text', stripped off all style information such as
-- colours.
prettyprintPlain :: Pretty a => a -> Text
prettyprintPlain = prettyprintModified plain

prettyprintModified :: Pretty a => (Doc -> Doc) -> a -> Text
prettyprintModified modifier input =
    T.pack (displayS (renderPretty 0.4 1000 (modifier (pretty input))) "")
