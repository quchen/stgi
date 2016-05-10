{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Various testing utilities.
module Test.Util (
    scaled,
    allEnums,
    arbitrary1,
    arbitrary2,
    arbitrary3,

    (==*==),
) where



import           Data.Ratio
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Test.QuickCheck
import           Test.UtilTH



-- | Scale the size parameter of a Quickcheck generator by a 'Ratio'. Useful
-- to implement exponential cutoff for recursive generators.
scaled
    :: Ratio Int
    -> Gen a
    -> Gen a
scaled factor = scale (\n -> n * numerator factor `quot` denominator factor)

allEnums :: (Enum a, Bounded a) => Gen a
allEnums = elements [minBound ..]

arbitrary1 :: $(arbitraryNType 1)
arbitrary1 = $(arbitraryNValue 1)

arbitrary2 :: $(arbitraryNType 2)
arbitrary2 = $(arbitraryNValue 2)

arbitrary3 :: $(arbitraryNType 3)
arbitrary3 = $(arbitraryNValue 3)

infix 4 ==*==
(==*==) :: (Eq a, Pretty a) => a -> a -> Property
x ==*== y = counterexample example (x == y)
  where
    example = (show . align . vsep) [pretty x, "is not equal to", pretty y]
