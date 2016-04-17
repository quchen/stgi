{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Stg.Parser.QuasiQuoter (

    -- * Heuristic quasiquoter
    stg,

    -- * Soecific syntax element quasiquoters
    stgProgram,
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



import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Lift
import           Language.Haskell.TH.Quote
import           Text.Megaparsec.Text

import           Stg.Parser.Parser


defaultQuoter :: QuasiQuoter
defaultQuoter = QuasiQuoter
    { quoteExp  = \_ -> fail "No STG expression quoter implemented"
    , quotePat  = \_ -> fail "No STG pattern quoter implemented"
    , quoteType = \_ -> fail "No STG type quoter implemented"
    , quoteDec  = \_ -> fail "No STG declaration quoter implemented" }

-- | Heuristic quasiquoter for STG language elements.
-- Tries a number of parsers, and will use the first successful one.
--
-- To gain more fine-grained control over what the input should be parsed to,
-- use one of the non-heuristic quoters, such as 'stgProgram' or
-- 'stgLambdaForm'.
--
-- >>> [stg| id = () \n (x) -> x () |]
-- Program (Binds [("id",LambdaForm [] NoUpdate ["x"] (AppF "x" []))])
--
-- >>> [stg | () \n (x) -> x () |]
-- LambdaForm [] NoUpdate ["x"] (AppF "x" [])
--
-- >>> [stg | x () |]
-- AppF "x" []
stg :: QuasiQuoter
stg = defaultQuoter { quoteExp  = expQuoter }
  where
    expQuoter input =
        let inputText = T.pack input
            parses =
                [ quoteAs program       inputText
                , quoteAs lambdaForm    inputText
                , quoteAs expr          inputText
                , quoteAs alts          inputText
                , quoteAs algebraicAlts inputText
                , quoteAs primitiveAlts inputText
                , quoteAs algebraicAlt  inputText
                , quoteAs primitiveAlt  inputText
                , quoteAs defaultAlt    inputText
                , quoteAs literal       inputText
                , quoteAs primOp        inputText
                , quoteAs vars          inputText
                , quoteAs atoms         inputText
                , quoteAs atom          inputText
                , quoteAs varTok        inputText
                , quoteAs conTok        inputText ]
        in case firstRight parses of
            Just ast -> ast
            Nothing  -> fail "No parse succeeded; try using a type-specific \
                             \parser, such as 'stgProgram'."

    firstRight :: [Either l r] -> Maybe r
    firstRight = listToMaybe . rights

    -- | Attempt to parse an input using a certain parser, and return the
    -- generated expression on success.
    quoteAs :: Lift ast => Parser ast -> Text -> Either Text (Q Exp)
    quoteAs p inputText = fmap lift (parse (spaceConsumer *> p) inputText)

-- | Build a quasiquoter from a 'Parser'.
stgQQ :: Lift ast => Parser ast -> QuasiQuoter
stgQQ parser = defaultQuoter { quoteExp  = expQuoter }
    where
    expQuoter input = case parse (spaceConsumer *> parser) (T.pack input) of
        Left err  -> fail ("Invalid STG program:\n" <> T.unpack err)
        Right ast -> [| ast |]

-- | Quasiquoter for 'Program's.
--
-- >>> [stgProgram| id = () \n (x) -> x () |]
-- Program (Binds [("id",LambdaForm [] NoUpdate ["x"] (AppF "x" []))])
stgProgram :: QuasiQuoter
stgProgram = stgQQ program

stgBinds :: QuasiQuoter
stgBinds = stgQQ binds

-- | Quasiquoter for 'LambdaForm's.
--
-- >>> [stgLambdaForm | () \n (x) -> x () |]
-- LambdaForm [] NoUpdate ["x"] (AppF "x" [])
stgLambdaForm :: QuasiQuoter
stgLambdaForm = stgQQ lambdaForm

-- | Quasiquoter for 'Expr'essions.
--
-- >>> [stgProgram| f (a,b,c) |]
-- AppF "x" [Var "a", Var "b", Var "c"]
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
