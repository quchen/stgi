{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.UtilTH (
    arbitraryN,
) where



import           GHC.Natural
import           Language.Haskell.TH

import           Test.QuickCheck



-- | Generate n-ary arbitrary chains.
--
-- @
-- arbitraryN n = \f -> f <$> arbitrary <*> arbitrary <*> ...
--                            ^------  n arbitraries  ------^
-- @
arbitraryN :: Natural -> ExpQ
arbitraryN n = lamE [varP f] [| $(chainOfArbitraries n) |]
  where
    chainOfArbitraries 0 = [| pure $(varE f) |]
    chainOfArbitraries m = [| $(chainOfArbitraries (m-1)) <*> arbitrary |]
    f = mkName "f"
