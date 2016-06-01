-- | Convert Haskell values to STG values and back.
--
-- This module is what users should be using - it reexports only the safe
-- classes.
module Stg.Marshal (
    ToStg(toStg),
    FromStg(fromStg),
    FromStgError(..),
) where

import Stg.Marshal.FromStg
import Stg.Marshal.ToStg
