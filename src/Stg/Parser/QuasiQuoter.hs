{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Quasiquoters for easier generation of STG syntax trees.
-- The 'stg' quoter is most convenient, I suggest you use it unless you have a
-- reason not to.
module Stg.Parser.QuasiQuoter (

    -- * Heuristic quasiquoter
    stg,

    -- * Specific syntax element quasiquoters
    stgProgram,
    stgBinds,
    stgLambdaForm,
    stgExpr,
    stgAlts,
    stgNonDefaultAlts,
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
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Lift
import           Language.Haskell.TH.Quote
import           Text.Megaparsec.Text

import Stg.Parser.Parser

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XQuasiQuotes


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
-- 'stgLambdaForm'. These will also give you much better error messages than
-- merely "doesn't work".
--
-- >>> [stg| id = () \n (x) -> x () |]
-- Program (Binds [(Var "id",LambdaForm [] NoUpdate [Var "x"] (AppF (Var "x") []))])
--
-- >>> [stg| () \n (x) -> x () |]
-- LambdaForm [] NoUpdate [Var "x"] (AppF (Var "x") [])
--
-- >>> [stg| x () |]
-- AppF (Var "x") []
stg :: QuasiQuoter
stg = defaultQuoter { quoteExp  = expQuoter }
  where
      -- TODO return all parse errors, not just "doesn't work"
    expQuoter input =
        let inputText = T.pack input
            parses =
                [ quoteAs "program"        program        inputText
                , quoteAs "lambdaForm"     lambdaForm     inputText
                , quoteAs "expr"           expr           inputText
                , quoteAs "alts"           alts           inputText
                , quoteAs "nonDefaultAlts" nonDefaultAlts inputText
                , quoteAs "algebraicAlt"   algebraicAlt   inputText
                , quoteAs "primitiveAlt"   primitiveAlt   inputText
                , quoteAs "defaultAlt"     defaultAlt     inputText
                , quoteAs "literal"        literal        inputText
                , quoteAs "primOp"         primOp         inputText
                , quoteAs "vars"           vars           inputText
                , quoteAs "atoms"          atoms          inputText
                , quoteAs "atom"           atom           inputText
                , quoteAs "varTok"         varTok         inputText
                , quoteAs "conTok"         conTok         inputText ]
        in case partitionEithers parses of
            (_, ast:_) -> ast
            (errs, _) -> (fail . T.unpack . T.unlines)
                ("No parse succeeded. Individual errors:" : errs)

    -- | Attempt to parse an input using a certain parser, and return the
    -- generated expression on success.
    quoteAs :: Lift ast => Text -> Parser ast -> Text -> Either Text (Q Exp)
    quoteAs parserName p inputText = fmap lift (case parse p inputText of
        Left err -> (Left . ("  - " <>) . T.unwords . T.words) (parserName <> ": " <> err)
        r@Right{} -> r )

-- | Build a quasiquoter from a 'Parser'.
stgQQ
    :: Lift ast
    => Parser ast -- ^ Parser to use
    -> Text       -- ^ Name of the parsed syntax element (for error reporting)
    -> QuasiQuoter
stgQQ parser elementName = defaultQuoter { quoteExp  = expQuoter }
    where
    expQuoter input = case parse parser (T.pack input) of
        Left err  -> fail (T.unpack ("Invalid STG " <> elementName <> ":\n" <> err))
        Right ast -> [| ast |]

-- | Quasiquoter for 'Stg.Language.Program's.
--
-- >>> [stgProgram| id = () \n (x) -> x () |]
-- Program (Binds [(Var "id",LambdaForm [] NoUpdate [Var "x"] (AppF (Var "x") []))])
stgProgram :: QuasiQuoter
stgProgram = stgQQ program "program"

-- | Quasiquoter for 'Stg.Language.Binds'.
--
-- >>> [stgBinds| id = () \n (x) -> x () |]
-- (Binds [(Var "id",LambdaForm [] NoUpdate [Var "x"] (AppF (Var "x") []))])
stgBinds :: QuasiQuoter
stgBinds = stgQQ binds "binds"

-- | Quasiquoter for 'Stg.Language.LambdaForm's.
--
-- >>> [stgLambdaForm| () \n (x) -> x () |]
-- LambdaForm [] NoUpdate [Var "x"] (AppF (Var "x") [])
stgLambdaForm :: QuasiQuoter
stgLambdaForm = stgQQ lambdaForm "lambda form"

-- | Quasiquoter for 'Stg.Language.Expr'essions.
--
-- >>> [stgExpr| f (x,y,z) |]
-- AppF (Var "f") [AtomVar (Var "x"),AtomVar (Var "y"),AtomVar (Var "z")]
stgExpr :: QuasiQuoter
stgExpr = stgQQ expr "expression"

-- | Quasiquoter for 'Stg.Language.Alts'.
--
-- >>> [stgAlts| Just (x) -> True (); default -> False () |]
-- Alts [AlgebraicAlt (Constr "Just") [Var "x"] (AppC (Constr "True") [])] (DefaultNotBound (AppC (Constr "False") []))
--
-- >>> [stgAlts| 0# -> True (); default -> False () |]
-- Alts [PrimitiveAlt (Literal 0) (AppC (Constr "True") [])] (DefaultNotBound (AppC (Constr "False") []))
stgAlts :: QuasiQuoter
stgAlts = stgQQ alts "alternatives"

-- | Quasiquoter for 'Stg.Language.Alt'.
--
-- >>>[stgNonDefaultAlts| Just (x) -> True (); Nothing () -> False (); |]
-- [AlgebraicAlt (Constr "Just") [Var "x"] (AppC (Constr "True") []),AlgebraicAlt (Constr "Nothing") [] (AppC (Constr "False") [])]
--
-- >>>[stgNonDefaultAlts| 0# -> False (); 1# -> True (); |]
-- [PrimitiveAlt (Literal 0) (AppC (Constr "False") []),PrimitiveAlt (Literal 1) (AppC (Constr "True") [])]
stgNonDefaultAlts :: QuasiQuoter
stgNonDefaultAlts = stgQQ nonDefaultAlts "algebraic alternatives"

-- | Quasiquoter for 'Stg.Language.AlgebraicAlt's.
--
-- >>> [stgAlgebraicAlt| Just (x) -> x (); |]
-- AlgebraicAlt (Constr "Just") [Var "x"] (AppF (Var "x") [])
stgAlgebraicAlt :: QuasiQuoter
stgAlgebraicAlt = stgQQ algebraicAlt "algebraic alternative"

-- | Quasiquoter for 'Stg.Language.PrimitiveAlt's.
--
-- >>> [stgPrimitiveAlt| 1# -> x (); |]
-- PrimitiveAlt (Literal 1) (AppF (Var "x") [])
stgPrimitiveAlt :: QuasiQuoter
stgPrimitiveAlt = stgQQ primitiveAlt "primitive alternative"

-- | Quasiquoter for 'Stg.Language.DefaultAlt's.
--
-- >>> [stgDefaultAlt| default -> x () |]
-- DefaultNotBound (AppF (Var "x") [])
--
-- >>> [stgDefaultAlt| x -> x () |]
-- DefaultBound (Var "x") (AppF (Var "x") [])
stgDefaultAlt :: QuasiQuoter
stgDefaultAlt = stgQQ defaultAlt "default alternative"

-- | Quasiquoter for 'Stg.Language.Literal's.
--
-- >>> [stgLiteral| 1# |]
-- Literal 1
stgLiteral :: QuasiQuoter
stgLiteral = stgQQ literal "literal"

-- | Quasiquoter for 'Stg.Language.PrimOp's.
--
-- >>> [stgPrimOp| +# |]
-- Add
stgPrimOp :: QuasiQuoter
stgPrimOp = stgQQ primOp "primop"

-- | Quasiquoter for @['Stg.Language.Var']@.
--
-- >>> [stgVars| (x,y,z) |]
-- [Var "x",Var "y",Var "z"]
stgVars :: QuasiQuoter
stgVars = stgQQ vars "variable list"

-- | Quasiquoter for @['Stg.Language.Atom']@.
--
-- >>> [stgAtoms| (x,y,z) |]
-- [AtomVar (Var "x"),AtomVar (Var "y"),AtomVar (Var "z")]
stgAtoms :: QuasiQuoter
stgAtoms = stgQQ atoms "atom list"

-- | Quasiquoter for 'Stg.Language.Atom's.
--
-- >>> [stgAtom| x |]
-- AtomVar (Var "x")
stgAtom :: QuasiQuoter
stgAtom = stgQQ atom "atom"
