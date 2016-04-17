{-# LANGUAGE OverloadedStrings #-}

-- | Various testing utilities.
module Test.Util (
    scaled,
    allEnums,
    arbitrary1,
    arbitrary2,
    arbitrary3,
    arbitrary4,

    (==*==),
) where



import           Data.Ratio
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Test.QuickCheck



-- | Scale the size parameter of a Quickcheck generator by a 'Ratio'. Useful
-- to implement exponential cutoff for recursive generators.
scaled
    :: Ratio Int
    -> Gen a
    -> Gen a
scaled factor = scale (\n -> n * numerator factor `quot` denominator factor)

allEnums :: (Enum a, Bounded a) => Gen a
allEnums = elements [minBound ..]

arbitrary1
    :: Arbitrary a
    => (a -> g)
    -> Gen g
arbitrary1 f = f <$> arbitrary

arbitrary2
    :: (Arbitrary a, Arbitrary b)
    => (a -> b -> g)
    -> Gen g
arbitrary2 f = f <$> arbitrary <*> arbitrary

arbitrary3
    :: (Arbitrary a, Arbitrary b, Arbitrary c)
    => (a -> b -> c -> g)
    -> Gen g
arbitrary3 f = f <$> arbitrary <*> arbitrary <*> arbitrary

arbitrary4
    :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => (a -> b -> c -> d -> g)
    -> Gen g
arbitrary4 f = f <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

infix 4 ==*==
(==*==) :: (Eq a, Pretty a) => a -> a -> Property
x ==*== y = counterexample example (x == y)
  where
    example = (show . align . vsep) [pretty x, "is not equal to", pretty y]
