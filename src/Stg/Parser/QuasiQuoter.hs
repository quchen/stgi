{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stg.Parser.QuasiQuoter (stg) where



import qualified Data.Text                 as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Stg.Parser.Parser         (parse, program)



stg :: QuasiQuoter
stg = QuasiQuoter
    { quoteExp  = stgExpQuoter
    , quotePat  = \_ -> fail "No STG pattern quoter implemented"
    , quoteType = \_ -> fail "No STG type quoter implemented"
    , quoteDec  = \_ -> fail "No STG declaration quoter implemented" }

stgExpQuoter :: String -> Q Exp
stgExpQuoter stgSource = case parse program (T.pack stgSource) of
    Left err  -> fail (T.unpack err)
    Right ast -> [| ast |]
