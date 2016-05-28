{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Quasiquoters for easier generation of STG syntax trees.
-- The 'stg' quoter is most convenient, I suggest you use it unless you have a
-- reason not to.
module Stg.Parser.QuasiQuoter (

    -- * Heuristic quasiquoter
    stg,

    -- * Specific syntax element quasiquoters
    program,
    binds,
    lambdaForm,
    expr,
    alts,
    nonDefaultAlts,
    algebraicAlt,
    primitiveAlt,
    defaultAlt,
    literal,
    primOp,
    atom,
) where



import           Data.Either
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Lift
import           Language.Haskell.TH.Quote
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import           Stg.Language.Prettyprint
import           Stg.Parser.Parser        (StgParser, parse)
import qualified Stg.Parser.Parser        as Parser
import qualified Stg.Parser.StgParserType as Parser

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
-- >>> [stg| id = \x -> x |]
-- Program (Binds [(Var "id",LambdaForm [] NoUpdate [Var "x"] (AppF (Var "x") []))])
--
-- >>> [stg| \x -> x |]
-- LambdaForm [] NoUpdate [Var "x"] (AppF (Var "x") [])
--
-- >>> [stg| x |]
-- AppF (Var "x") []
stg :: QuasiQuoter
stg = defaultQuoter { quoteExp = expQuoter }
  where
    expQuoter inputString =
        let input = T.pack inputString
            parses =
                [ quoteAs "program"        Parser.program        input
                , quoteAs "lambdaForm"     Parser.lambdaForm     input
                , quoteAs "expr"           Parser.expr           input
                , quoteAs "alts"           Parser.alts           input
                , quoteAs "algebraicAlt"   Parser.algebraicAlt   input
                , quoteAs "primitiveAlt"   Parser.primitiveAlt   input
                , quoteAs "defaultAlt"     Parser.defaultAlt     input
                , quoteAs "literal"        Parser.literal        input
                , quoteAs "primOp"         Parser.primOp         input
                , quoteAs "atom"           Parser.atom           input
                , quoteAs "variable"       Parser.var            input
                , quoteAs "constructor"    Parser.con            input ]
        in case partitionEithers parses of
            (_, ast:_) -> ast
            (errs, _) -> (fail . T.unpack . T.unlines)
                ("No parse succeeded. Individual errors:" : errs)

    -- | Attempt to parse an input using a certain parser, and return the
    -- generated expression on success.
    quoteAs :: Lift ast => Text -> Parser.StgParser ast -> Text -> Either Text (Q Exp)
    quoteAs parserName parser input = fmap lift (case Parser.parse parser input of
        Left err -> Left (prettyprint ("  -" <+> text (T.unpack parserName) <> ":" <+> plain (align err)))
        Right r -> Right r )

-- | Build a quasiquoter from a 'Parser'.
stgQQ
    :: Lift ast
    => StgParser ast
    -> Text -- ^ Name of the parsed syntax element (for error reporting)
    -> QuasiQuoter
stgQQ parser elementName = defaultQuoter { quoteExp  = expQuoter }
    where
    expQuoter input = case parse parser (T.pack input) of
        Left err  -> fail (T.unpack ("Invalid STG " <> elementName <> ":\n" <> prettyprint (plain err)))
        Right ast -> [| ast |]

-- | Quasiquoter for 'Stg.Language.Program's.
--
-- >>> [program| id = \x -> x |]
-- Program (Binds [(Var "id",LambdaForm [] NoUpdate [Var "x"] (AppF (Var "x") []))])
program :: QuasiQuoter
program = stgQQ Parser.program "program"

-- | Quasiquoter for 'Stg.Language.Binds'.
--
-- >>> [binds| id = \x -> x |]
-- (Binds [(Var "id",LambdaForm [] NoUpdate [Var "x"] (AppF (Var "x") []))])
binds :: QuasiQuoter
binds = stgQQ Parser.binds "binds"

-- | Quasiquoter for 'Stg.Language.LambdaForm's.
--
-- >>> [lambdaForm| \x -> x |]
-- LambdaForm [] NoUpdate [Var "x"] (AppF (Var "x") [])
lambdaForm :: QuasiQuoter
lambdaForm = stgQQ Parser.lambdaForm "lambda form"

-- | Quasiquoter for 'Stg.Language.Expr'essions.
--
-- >>> [expr| f x y z |]
-- AppF (Var "f") [AtomVar (Var "x"),AtomVar (Var "y"),AtomVar (Var "z")]
expr :: QuasiQuoter
expr = stgQQ Parser.expr "expression"

-- | Quasiquoter for 'Stg.Language.Alts'.
--
-- >>> [alts| Just x -> True; default -> False |]
-- Alts (AlgebraicAlts (AlgebraicAlt (Constr "Just") [Var "x"] (AppC (Constr "True") []) :| [])) (DefaultNotBound (AppC (Constr "False") []))
--
-- >>> [alts| 0# -> True; default -> False |]
-- Alts (PrimitiveAlts (PrimitiveAlt (Literal 0) (AppC (Constr "True") []) :| [])) (DefaultNotBound (AppC (Constr "False") []))
alts :: QuasiQuoter
alts = stgQQ (Parser.nesting Parser.alts) "alternatives"

-- | Quasiquoter for 'Stg.Language.Alt'.
--
-- >>> [nonDefaultAlts| Just x -> True; Nothing -> False; |]
-- AlgebraicAlts (AlgebraicAlt (Constr "Just") [Var "x"] (AppC (Constr "True") []) :| [AlgebraicAlt (Constr "Nothing") [] (AppC (Constr "False") [])])
--
-- >>> [nonDefaultAlts| 0# -> False; 1# -> True; |]
-- PrimitiveAlts (PrimitiveAlt (Literal 0) (AppC (Constr "False") []) :| [PrimitiveAlt (Literal 1) (AppC (Constr "True") [])])
nonDefaultAlts :: QuasiQuoter
nonDefaultAlts = stgQQ Parser.nonDefaultAlts "algebraic alternatives"

-- | Quasiquoter for 'Stg.Language.AlgebraicAlt's.
--
-- >>> [algebraicAlt| Just x -> x; |]
-- AlgebraicAlt (Constr "Just") [Var "x"] (AppF (Var "x") [])
algebraicAlt :: QuasiQuoter
algebraicAlt = stgQQ Parser.algebraicAlt "algebraic alternative"

-- | Quasiquoter for 'Stg.Language.PrimitiveAlt's.
--
-- >>> [primitiveAlt| 1# -> x; |]
-- PrimitiveAlt (Literal 1) (AppF (Var "x") [])
primitiveAlt :: QuasiQuoter
primitiveAlt = stgQQ Parser.primitiveAlt "primitive alternative"

-- | Quasiquoter for 'Stg.Language.DefaultAlt's.
--
-- >>> [defaultAlt| default -> x |]
-- DefaultNotBound (AppF (Var "x") [])
--
-- >>> [defaultAlt| x -> x |]
-- DefaultBound (Var "x") (AppF (Var "x") [])
defaultAlt :: QuasiQuoter
defaultAlt = stgQQ Parser.defaultAlt "default alternative"

-- | Quasiquoter for 'Stg.Language.Literal's.
--
-- >>> [literal| 1# |]
-- Literal 1
literal :: QuasiQuoter
literal = stgQQ Parser.literal "literal"

-- | Quasiquoter for 'Stg.Language.PrimOp's.
--
-- >>> [primOp| +# |]
-- Add
primOp :: QuasiQuoter
primOp = stgQQ Parser.primOp "primop"

-- | Quasiquoter for 'Stg.Language.Atom's.
--
-- >>> [atom| x |]
-- AtomVar (Var "x")
atom :: QuasiQuoter
atom = stgQQ Parser.atom "atom"
