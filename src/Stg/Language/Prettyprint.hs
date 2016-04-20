{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Prettyprinting STG elements in various formats.
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



-- | Prettyprint a value as 'Text'.
prettyprint :: Pretty a => a -> Text
prettyprint = asText pretty

-- | Prettyprint a value as 'Text', in a format that is compatible with the
-- "Stg.Parser".
prettyprintParserInverse :: PrettyParserInverse a => a -> Text
prettyprintParserInverse = asText pprPI

-- | Prettyprint a value as 'Text', with ANSI terminal code based formatting.
prettyprintAnsi :: PrettyAnsi a => a -> Text
prettyprintAnsi = asText prettyAnsi

asText :: (a -> Doc) -> a -> Text
asText prettyprinter input =
    T.pack (displayS (renderPretty 0.4 1000 (prettyprinter input)) "")
