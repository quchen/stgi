{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stg.Language.Prettyprint (

    -- * Inverse-of-parser prettyprinter
    Pretty,
    prettyprint,

    -- * Inverse-of-parser prettyprinter
    prettyParserInverse,
    PrettyParserInverse,

    -- * ANSI terminal styled
    prettyParserInverseAnsi,
    PrettyParserInverseAnsi,

) where



import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           Prelude                                    hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

import           Stg.Language.Prettyprint.ParserInverse
import           Stg.Language.Prettyprint.ParserInverseAnsi



prettyprint :: Pretty a => a -> Text
prettyprint = asText pretty

prettyParserInverse :: PrettyParserInverse a => a -> Text
prettyParserInverse = asText pprPI

prettyParserInverseAnsi :: PrettyParserInverseAnsi a => a -> Text
prettyParserInverseAnsi = asText pprAnsi

asText :: (a -> Doc) -> a -> Text
asText prettyprinter input =
    T.pack (displayS (renderPretty 0.4 80 (prettyprinter input)) "")
