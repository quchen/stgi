{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stg.Language.Prettyprint (
    parserInverse,
    parserInverseAnsi,
    PrettyprinterDict(..),
) where



import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           Prelude                                    hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

import           Stg.Language.Prettyprint.Module
import           Stg.Language.Prettyprint.ParserInverse
import           Stg.Language.Prettyprint.ParserInverseAnsi



-- |
-- >>> let prog = [stg| fix = () \n (f) -> letrec x = (f,x) \n () -> f (x) in x ()|]
-- >>> parserInverse pprProgram prog
-- fix = () \n (f) -> letrec x = (f, x) \n () -> f (x)
--                    in x ()
parserInverse
    :: (PrettyprinterDict Doc -> input -> Doc)
    -> input
    -> Text
parserInverse = makeDocPrettyprinter parserInverseModule

-- | ANSI terminal coloured version of 'parserInverse'.
parserInverseAnsi
    :: (PrettyprinterDict Doc -> input -> Doc)
    -> input
    -> Text
parserInverseAnsi = makeDocPrettyprinter parserInverseColouredModule

-- | Create a prettyprinter given a 'PrettyprinterModule' and an accessor
-- to the contained prettyprinter.
makeDocPrettyprinter
    :: PrettyprinterModule out
        -- ^ Prettyprinter module, containing a description of how to build a
        --   prettyprinter
    -> (PrettyprinterDict out -> input -> Doc)
        -- ^ Projection function to get a prettyprinting function for a
        --   specific type out of a 'PrettyprinterDict'

    -> input
        -- ^ Input to prettyprint

    -> Text
makeDocPrettyprinter pprModule getter input =
    let ppr = getter (makePrettyprinter pprModule)
    in T.pack (displayS (renderPretty 0.4 80 (ppr input)) "")
