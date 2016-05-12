{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Stg.Language.Prelude.Number (
    -- * Convenience
    int,

    -- * Arithmetic
    add,
    sub,
    mul,
    div,
    mod,

    -- * Comparisons
    eq,
    lt,
    leq,
    gt,
    geq,
    neq,
) where



import Prelude (Integer)

import Data.Monoid ((<>))
import Data.Text   (Text)

import Stg.Language
import Stg.Parser

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XQuasiQuotes
-- >>> :module +Stg.Language.Prettyprint



-- | Boxed int generator to abbreviate simple number generation.
--
-- >>> prettyprint (int "one" 1)
-- "one = () \\n () -> Int# (1#)"
int :: Text -> Integer -> Program
int name i = Program (Binds [(Var name, LambdaForm [] NoUpdate []
    (AppC (Constr "Int#") [AtomLit (Literal i)]) )])



binaryOp :: Text -> PrimOp -> Alts -> Program
binaryOp name op primAlts =
    Program (Binds
        [(Var name, LambdaForm [] NoUpdate [Var "x", Var "y"]
            (Case (AppF (Var "x") []) (Alts
                [AlgebraicAlt (Constr "Int#") [Var "x'"]
                    (Case (AppF (Var "y") []) (Alts
                        [AlgebraicAlt (Constr "Int#") [Var "y'"]
                            (Case (AppP op (AtomVar (Var "x'")) (AtomVar (Var "y'")))
                                primAlts) ]
                        (DefaultBound (Var "err") (AppC (Constr ("Error_" <> name <> "_1")) [AtomVar (Var "err")])) ))]
                (DefaultBound (Var "err") (AppC (Constr ("Error_" <> name <> "_2")) [AtomVar (Var "err")])) )))])



primToBool :: Alts
primToBool = [stgAlts| 1# -> True (); default -> False () |]

add, sub, mul, div, mod :: Program

-- |
eq  = binaryOp "eq_Int"  Eq  primToBool

-- |
lt  = binaryOp "lt_Int"  Lt  primToBool

-- |
leq = binaryOp "leq_Int" Leq primToBool

-- |
gt  = binaryOp "gt_Int"  Gt  primToBool

-- |
geq = binaryOp "geq_Int" Geq primToBool

-- |
neq = binaryOp "neq_Int" Neq primToBool



primIdInt :: Alts
primIdInt = [stgAlts| v -> Int# (v) |]

eq, lt, leq, gt, geq, neq :: Program

-- |
add = binaryOp "add" Add primIdInt

-- |
sub = binaryOp "sub" Sub primIdInt

-- |
mul = binaryOp "mul" Mul primIdInt

-- |
div = binaryOp "div" Div primIdInt

-- |
mod = binaryOp "mod" Mod primIdInt
