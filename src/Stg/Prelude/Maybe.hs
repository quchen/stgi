{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Stg.Prelude.Maybe (
    nothing,
) where



import Prelude ()

import Stg.Language
import Stg.Parser.QuasiQuoter



nothing :: Program



-- | 'Nothing' as a top-level closure.
--
-- @
-- nothing : Maybe a
-- @
nothing = [program| nothing = \ -> Nothing |]
