{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Functions as found in Haskell's "Data.Maybe" module.
--
-- This module should be imported qualified to avoid clashes with standard
-- Haskell definitions.
module Stg.Prelude.Maybe (
    nothing,
    maybe,
) where



import Prelude ()

import Stg.Language
import Stg.Parser.QuasiQuoter



nothing, maybe :: Program



-- | 'Nothing' as a top-level closure.
--
-- @
-- nothing : Maybe a
-- @
nothing = [program| nothing = \ -> Nothing |]

-- | Deconstructor of the 'Maybe' type.
--
-- @
-- maybe : b -> (a -> b) -> Maybe a -> b
-- @
maybe = [program|
    maybe = \nothing just x -> case x of
        Just j   -> just j;
        Nothing  -> nothing;
        badMaybe -> Error_badMaybe badMaybe |]
