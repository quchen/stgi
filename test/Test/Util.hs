-- | Various testing utilities.
module Test.Util (
    scaled,
) where



import           Data.Ratio

import           Test.QuickCheck



-- | Scale the size parameter of a Quickcheck generator by a 'Ratio'. Useful
-- to implement exponential cutoff for recursive generators.
scaled
    :: Ratio Int
    -> Gen a
    -> Gen a
scaled factor = scale (\n -> n * numerator factor `quot` denominator factor)
