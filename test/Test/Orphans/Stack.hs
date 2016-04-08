{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Orphans.Stack () where



import           GHC.Exts
import           Test.Tasty.QuickCheck as QC

import           Stack                 (Stack)



instance Arbitrary a => Arbitrary (Stack a) where
    arbitrary = fmap fromList arbitrary
    shrink = map fromList . shrink . toList
