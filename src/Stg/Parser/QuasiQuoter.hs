{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stg.Parser.QuasiQuoter (stg) where



import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Stg.Parser.Parser         (parse, program)



-- | Quasiquoter for entering STG programs more conveniently.
--
-- >>> [stg| id = () \n (x) -> x () |]
-- Program (Binds [("id",LambdaForm [] NoUpdate ["x"] (AppF "x" []))])
stg :: QuasiQuoter
stg = QuasiQuoter
    { quoteExp  = stgExpQuoter . T.pack
    , quotePat  = \_ -> fail "No STG pattern quoter implemented"
    , quoteType = \_ -> fail "No STG type quoter implemented"
    , quoteDec  = \_ -> fail "No STG declaration quoter implemented" }

stgExpQuoter :: Text -> Q Exp
stgExpQuoter stgSource = case parse program stgSource of
    Left err  -> fail (T.unpack err)
    Right ast -> [| ast |]
