{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Operations on boxed integers
module Stg.Prelude.Number (
    -- * Arithmetic
    add,
    sub,
    mul,
    div,
    mod,

    -- * Comparisons
    eq_Int,
    lt_Int,
    leq_Int,
    gt_Int,
    geq_Int,
    neq_Int,

    -- * Other
    min,
    max,
) where



import Prelude ()

import Data.Monoid ((<>))
import Data.Text   (Text)

import Stg.Language
import Stg.Parser.QuasiQuoter

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XQuasiQuotes
-- >>> :module +Stg.Language.Prettyprint



binaryOp :: Text -> PrimOp -> Alts -> Program
binaryOp name op primAlts =
    Program (Binds
        [(Var name, LambdaForm [] NoUpdate [Var "x", Var "y"]
            (Case (AppF (Var "x") []) (Alts (AlgebraicAlts
                [AlgebraicAlt (Constr "Int#") [Var "x'"]
                    (Case (AppF (Var "y") []) (Alts (AlgebraicAlts
                        [AlgebraicAlt (Constr "Int#") [Var "y'"]
                            (Case (AppP op (AtomVar (Var "x'")) (AtomVar (Var "y'")))
                                primAlts) ])
                        (DefaultBound (Var "err") (AppC (Constr ("Error_" <> name <> "_1")) [AtomVar (Var "err")])) ))])
                (DefaultBound (Var "err") (AppC (Constr ("Error_" <> name <> "_2")) [AtomVar (Var "err")])) )))])



primToBool :: Alts
primToBool = [alts| 1# -> True; default -> False |]

eq_Int, lt_Int, leq_Int, gt_Int, geq_Int, neq_Int :: Program

-- | Equality of boxed integers
eq_Int = binaryOp "eq_Int" Eq primToBool

-- | Less-than for boxed integers
lt_Int = binaryOp "lt_Int" Lt primToBool

-- | Less-or-equal for boxed integers
leq_Int = binaryOp "leq_Int" Leq primToBool

-- | Greater-than for boxed integers
gt_Int = binaryOp "gt_Int" Gt primToBool

-- | Greater-or-equal for boxed integers
geq_Int = binaryOp "geq_Int" Geq primToBool

-- | Inequality of boxed integers
neq_Int = binaryOp "neq_Int" Neq primToBool



primIdInt :: Alts
primIdInt = [alts| v -> Int# v |]

add, sub, mul, div, mod :: Program

-- | Binary addition of boxed integers
add = binaryOp "add" Add primIdInt

-- | Difference of boxed integers
sub = binaryOp "sub" Sub primIdInt

-- | Binary multiplication of boxed integers
mul = binaryOp "mul" Mul primIdInt

-- | Boxed integer division
div = binaryOp "div" Div primIdInt

-- | Boxed integer modulo operator
mod = binaryOp "mod" Mod primIdInt

-- | Minimum of two boxed integers
min :: Program
min = [program|
    min = \x y -> case x of
        Int# x' -> case y of
            Int# y' -> case <=# x' y' of
                1#      -> x;
                default -> y;
            badInt -> Error_min badInt;
        badInt -> Error_min badInt
    |]

-- | Maximum of two boxed integers
max :: Program
max = [program|
    max = \x y -> case x of
        Int# x' -> case y of
            Int# y' -> case >=# x' y' of
                1# -> x;
                default -> y;
            badInt -> Error_min badInt;
        badInt -> Error_min badInt
    |]
