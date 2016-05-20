-- | Common Haskell functions, translated to STG. Use the 'Monoid' instance
-- for 'Program' to mix them.
--
-- This module should be imported qualified, since it heavily conflicts with the
-- standard Haskell "Prelude".
module Stg.Prelude (
    -- * Lists
    nil,
    concat2,
    reverse,
    foldl,
    foldl',
    foldr,
    iterate,
    cycle,
    take,
    filter,
    repeat,
    replicate,
    sort,
    map,
    length,
    zip,
    zipWith,

    -- * Tuples
    fst,
    snd,
    curry,
    uncurry,
    swap,

    -- * Boolean
    and2,
    or2,
    not,
    bool,
    eq_Bool,
    boolValue,

    -- ** Convenience
    listOfNumbers,
    equals_List_Int,
    tupleOfNumbers,
    equals_Tuple_Int,

    -- * Numbers
    -- ** Convenience
    int,
    -- ** Arithmetic
    add,
    sub,
    mul,
    div,
    mod,
    -- ** Comparisons
    eq_Int,
    lt_Int,
    leq_Int,
    gt_Int,
    geq_Int,
    neq_Int,

    -- ** Other
    min,
    max,

    -- * Functions
    seq,
    id,
    const,
    compose,
    fix,
) where



import Prelude ()

import Stg.Prelude.Bool
import Stg.Prelude.Function
import Stg.Prelude.List
import Stg.Prelude.Number
import Stg.Prelude.Tuple
