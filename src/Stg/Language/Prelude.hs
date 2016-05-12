-- | Common Haskell functions, translated to STG. Use the 'Monoid' instance
-- for 'Program' to mix them.
--
-- This module should be imported qualified, since it heavily conflicts with the
-- standard Haskell "Prelude".
module Stg.Language.Prelude (
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
    eq,
    lt,
    leq,
    gt,
    geq,
    neq,

    -- * Functions
    seq,
    id,
    const,
    compose,
    fix,
) where



import Prelude ()

import Stg.Language.Prelude.Bool
import Stg.Language.Prelude.Function
import Stg.Language.Prelude.List
import Stg.Language.Prelude.Number
import Stg.Language.Prelude.Tuple
