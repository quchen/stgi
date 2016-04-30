-- | Common Haskell functions, translated to STG. Use the 'Monoid' instance
-- for 'Program' to mix them.
--
-- This module should be imported qualified, since it heavily conflicts with the
-- standard Haskell "Prelude".
module Stg.Language.Prelude (
    -- * Lists
    nil,
    concat,
    foldl,
    foldl',
    foldr,
    iterate,
    cycle,
    take,
    filter,
    repeat,
    sort,
    map,
    listIntEquals,

    -- * Boolean
    and2,
    or2,
    not,

    -- ** Convenience
    listOfNumbers,

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



import           Prelude                       ()

import           Stg.Language.Prelude.Function
import           Stg.Language.Prelude.List
import           Stg.Language.Prelude.Bool
import           Stg.Language.Prelude.Number
