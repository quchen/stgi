{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Orphans.Stack () where



import           GHC.Exts
import           Test.Tasty.QuickCheck as QC

import           Stack                 (Stack)

import           Test.Util



instance Arbitrary a => Arbitrary (Stack a) where
    arbitrary = arbitrary1 fromList
    shrink = map fromList . shrink . toList
