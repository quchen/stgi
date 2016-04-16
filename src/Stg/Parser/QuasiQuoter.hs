{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stg.Parser.QuasiQuoter (
    -- * Program quasiquoter
    stg,

    -- * Syntax element quasiquoters
    stgBinds,
    stgLambdaForm,
    stgExpr,
    stgAlts,
    stgAlgebraicAlts,
    stgPrimitiveAlts,
    stgAlgebraicAlt,
    stgPrimitiveAlt,
    stgDefaultAlt,
    stgLiteral,
    stgPrimOp,
    stgVars,
    stgAtoms,
    stgAtom,
) where



import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Lift
import           Language.Haskell.TH.Quote
import           Text.Megaparsec.Text

import           Stg.Parser.Parser



stgExpQuoter :: Lift ast => Parser ast -> Text -> Q Exp
stgExpQuoter parser stgSource = case parse parser stgSource of
    Left err  -> fail ("Invalid STG program:\n" <> T.unpack err)
    Right ast -> [| ast |]

-- | Build a quasiquoter from a 'Parser'.
stgQQ :: Lift ast => Parser ast -> QuasiQuoter
stgQQ parser = QuasiQuoter
    { quoteExp  = stgExpQuoter (spaceConsumer *> parser) . T.pack
    , quotePat  = \_ -> fail "No STG pattern quoter implemented"
    , quoteType = \_ -> fail "No STG type quoter implemented"
    , quoteDec  = \_ -> fail "No STG declaration quoter implemented" }

-- | Quasiquoter for 'Program's.
--
-- >>> [stg| id = () \n (x) -> x () |]
-- Program (Binds [("id",LambdaForm [] NoUpdate ["x"] (AppF "x" []))])
stg :: QuasiQuoter
stg = stgQQ program





stgBinds :: QuasiQuoter
stgBinds = stgQQ binds

-- | Quasiquoter for 'LambdaForm's.
--
-- >>> [stgLambdaForm | () \n (x) -> x () |]
-- LambdaForm [] NoUpdate ["x"] (AppF "x" [])
stgLambdaForm :: QuasiQuoter
stgLambdaForm = stgQQ lambdaForm

stgExpr :: QuasiQuoter
stgExpr = stgQQ expr

stgAlts :: QuasiQuoter
stgAlts = stgQQ alts

stgAlgebraicAlts :: QuasiQuoter
stgAlgebraicAlts = stgQQ algebraicAlts

stgPrimitiveAlts :: QuasiQuoter
stgPrimitiveAlts = stgQQ primitiveAlts

stgAlgebraicAlt :: QuasiQuoter
stgAlgebraicAlt = stgQQ algebraicAlt

stgPrimitiveAlt :: QuasiQuoter
stgPrimitiveAlt = stgQQ primitiveAlt

stgDefaultAlt :: QuasiQuoter
stgDefaultAlt = stgQQ defaultAlt

stgLiteral :: QuasiQuoter
stgLiteral = stgQQ literal

stgPrimOp :: QuasiQuoter
stgPrimOp = stgQQ primOp

stgVars :: QuasiQuoter
stgVars = stgQQ vars

stgAtoms :: QuasiQuoter
stgAtoms = stgQQ atoms

stgAtom :: QuasiQuoter
stgAtom = stgQQ atom
