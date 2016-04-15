{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stg.Language.Prettyprint (

    -- * Inverse-of-parser prettyprinter
    Pretty(..),
    prettyprint,

    -- * Inverse-of-parser prettyprinter
    prettyprintParserInverse,
    PrettyParserInverse,

    -- * ANSI terminal styled
    prettyprintAnsi,
    PrettyAnsi,

) where



import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Prelude                                hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

import           Stg.Language.Prettyprint.Ansi
import           Stg.Language.Prettyprint.ParserInverse



prettyprint :: Pretty a => a -> Text
prettyprint = asText pretty

prettyprintParserInverse :: PrettyParserInverse a => a -> Text
prettyprintParserInverse = asText pprPI

prettyprintAnsi :: PrettyAnsi a => a -> Text
prettyprintAnsi = asText prettyAnsi

asText :: (a -> Doc) -> a -> Text
asText prettyprinter input =
    T.pack (displayS (renderPretty 0.4 80 (prettyprinter input)) "")
