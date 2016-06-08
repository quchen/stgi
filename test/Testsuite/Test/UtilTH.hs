{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.UtilTH (
    arbitraryN,
) where



import GHC.Natural
import Language.Haskell.TH

import Test.QuickCheck


arbitraryN :: Natural -> DecsQ
arbitraryN n = sequence [arbitraryNType n, arbitraryNValue n]



arbitraryName :: Natural -> Name
arbitraryName n = mkName ("arbitrary" ++ show n)



-- | Generate n-ary arbitrary chains.
--
-- @
-- arbitraryN n :: (Arbitrary a, Arbitrary b, ...) -> (a -> b -> ... -> g) -> Gen g
-- arbitraryN n = \f -> f <$> arbitrary <*> arbitrary <*> ...
--                            ^------  n arbitraries  ------^
-- @
arbitraryNValue :: Natural -> DecQ
arbitraryNValue n
  = valD (varP (arbitraryName n)) (normalB (lamE [varP f] [| $(chainOfArbitraries n) |])) []
  where
    chainOfArbitraries 0 = [| pure $(varE f) |]
    chainOfArbitraries 1 = [| $(varE f) <$> arbitrary |]
    chainOfArbitraries m = [| $(chainOfArbitraries (m-1)) <*> arbitrary |]
    f = mkName "f"



arbitraryNType :: Natural -> DecQ
arbitraryNType n
  = sigD (arbitraryName n) (forallT forallArgs constraints functionType)
  where
    forallArgs = map PlainTV (gName:names)
    constraints = cxt [[t| Arbitrary $(varT name)|] | name <- names]
    functionType = [t| $(foldr (\name rest -> [t|$(varT name) -> $(rest)|]) (varT gName) names)
                      -> Gen $(varT gName)|]
    names = take (fromIntegral n) [mkName [x] | x <- filter (/= 'g') ['a'..]]
    gName = mkName "g"
