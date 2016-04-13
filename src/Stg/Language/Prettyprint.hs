{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stg.Language.Prettyprint (
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


prettyParserInverse :: PrettyParserInverse a => a -> Text
prettyParserInverse input =
    T.pack (displayS (renderPretty 0.4 80 (pprPI input)) "")

prettyParserInverseAnsi :: PrettyParserInverseAnsi a => a -> Text
prettyParserInverseAnsi input =
    T.pack (displayS (renderPretty 0.4 80 (pprAnsi input)) "")
