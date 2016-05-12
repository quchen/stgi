{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.UtilTH (
    arbitraryNValue,
    arbitraryNType,
) where



import GHC.Natural
import Language.Haskell.TH

import Test.QuickCheck



-- | Generate n-ary arbitrary chains.
--
-- @
-- arbitraryN n = \f -> f <$> arbitrary <*> arbitrary <*> ...
--                            ^------  n arbitraries  ------^
-- @
arbitraryNValue :: Natural -> ExpQ
arbitraryNValue n = lamE [varP f] [| $(chainOfArbitraries n) |]
  where
    chainOfArbitraries 0 = [| pure $(varE f) |]
    chainOfArbitraries m = [| $(chainOfArbitraries (m-1)) <*> arbitrary |]
    f = mkName "f"

-- | Type of 'arbitraryNValue'.
--
-- @
-- arbitraryN n
-- :: (Arbitrary a, Arbitrary b, ...)
--    -> (a -> b -> ... -> g)
--    -> Gen g
-- @
arbitraryNType :: Natural -> TypeQ
arbitraryNType n = forallT forallArgs constraints functionType
  where
    forallArgs = PlainTV gName : [PlainTV name | name <- names]
    constraints = cxt [[t| Arbitrary $(varT name)|] | name <- names]
    functionType = [t| $(foldr (\name rest -> [t|$(varT name) -> $(rest)|]) (varT gName) names)
                      -> Gen $(varT gName)|]
    names = take (fromIntegral n) [mkName [x] | x <- filter (/= 'g') ['a'..]]
    gName = mkName "g"
