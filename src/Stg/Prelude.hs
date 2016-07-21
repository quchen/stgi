{-# LANGUAGE QuasiQuotes #-}


-- | Common Haskell functions, translated to STG. Use the 'Monoid' instance
-- for 'Program' to mix them.
--
-- This module should be imported qualified to avoid clashes with standard
-- Haskell definitions.
module Stg.Prelude (
    module Stg.Prelude,
    module Stg.Prelude.Bool,
    module Stg.Prelude.Function,
    module Stg.Prelude.List,
    module Stg.Prelude.Maybe,
    module Stg.Prelude.Number,
    module Stg.Prelude.Tuple,
) where



import Prelude ()

import Stg.Language
import Stg.Parser.QuasiQuoter
import Stg.Prelude.Bool
import Stg.Prelude.Function
import Stg.Prelude.List
import Stg.Prelude.Maybe
import Stg.Prelude.Number
import Stg.Prelude.Tuple

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
    -- Haddock doesn't like this trick :-/



-- | Force a value to normal form and return it.
--
-- This function makes heavy use of the fact that the STG is untyped. It
-- currently supports the following types:
--
--  * Unit (Unit)
--  * Maybe (Just, Nothing)
--  * Bool (True, False)
--  * Int (Int#)
--  * Either (Left, Right)
--  * Tuples (Pair, Triple)
--  * List (Nil, Cons)
--
-- Everything else will run into an error.
force :: Program
force = [stg|
    force = \ =>
        letrec
            go0 = \ -> Done;
            go1 = \(go go0) x     -> case go x of default -> go0;
            go2 = \(go go1) x y   -> case go x of default -> go1 y;
            go3 = \(go go2) x y z -> case go x of default -> go2 y z;

            go = \(go0 go1 go2 go3) x -> case x of

                Unit -> go0;

                Nothing -> go0;
                Just x  -> go1 x;

                True  -> go0;
                False -> go0;

                Int# _ -> go0;

                Left l  -> go1 l;
                Right r -> go1 r;

                Pair   x y   -> go2 x y;
                Triple x y z -> go3 x y z;

                Nil       -> go0;
                Cons x xs -> go2 x xs;

                x -> Error_ForceNotImplementedFor x;

            forceAndReturnValue = \(go) x -> case go x of default -> x
        in forceAndReturnValue |]
