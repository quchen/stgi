{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Orphans () where



import           GHC.Exts
import           Test.Tasty.QuickCheck as QC

import qualified Stack                 as S



instance Arbitrary a => Arbitrary (S.Stack a) where
    arbitrary = fmap fromList arbitrary
    shrink = map fromList . shrink . toList
