{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Orphans.Stack () where



import GHC.Exts

import Data.Stack (Stack)

import Test.Tasty.QuickCheck as QC
import Test.Util



instance Arbitrary a => Arbitrary (Stack a) where
    arbitrary = arbitrary_1 fromList
    shrink = map fromList . shrink . toList
