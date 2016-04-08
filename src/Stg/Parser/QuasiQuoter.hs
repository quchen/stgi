{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stg.Parser.QuasiQuoter (stg) where



import qualified Data.Text                 as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Stg.Parser.Parser         (parse)



stg :: QuasiQuoter
stg = QuasiQuoter
    { quoteExp  = stgExpQuoter
    , quotePat  = fail "No STG pattern quoter implemented"
    , quoteType = fail "No STG type quoter implemented"
    , quoteDec  = fail "No STG declaration quoter implemented" }

stgExpQuoter :: String -> Q Exp
stgExpQuoter stgSource = case parse (T.pack stgSource) of
    Left err  -> fail err
    Right ast -> [| ast |]
