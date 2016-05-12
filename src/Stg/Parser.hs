-- | Convert source code to syntax trees.
module Stg.Parser (

    -- * Parser
    parse,

    -- * Quasiquoters

    -- ** Heuristic quasiquoter
    stg,

    -- ** Specific syntax element quasiquoters
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



import           Data.Text              (Text)
import qualified Stg.Parser.Parser      as P
import           Stg.Parser.QuasiQuoter

import Stg.Language



-- | Parse an STG program.
parse :: Text -> Either Text Program
parse = P.parse P.program
