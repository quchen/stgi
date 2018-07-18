{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Various testing utilities.
module Test.Util (
    scaled,
    allEnums,
    arbitrary_0,
    arbitrary_1,
    arbitrary_2,
    arbitrary_3,

    (==*==),
) where



import Data.Ratio
import Data.Text.Prettyprint.Doc

import Stg.Language.Prettyprint

import Test.QuickCheck
import Test.UtilTH



-- | Scale the size parameter of a Quickcheck generator by a 'Ratio'. Useful
-- to implement exponential cutoff for recursive generators.
scaled
    :: Ratio Int
    -> Gen a
    -> Gen a
scaled factor = scale (\n -> n * numerator factor `quot` denominator factor)

allEnums :: (Enum a, Bounded a) => Gen a
allEnums = elements [minBound ..]

$(arbitrary_N 0)
$(arbitrary_N 1)
$(arbitrary_N 2)
$(arbitrary_N 3)

infix 4 ==*==
(==*==) :: (Eq a, PrettyStgi a) => a -> a -> Property
x ==*== y = counterexample example (x == y)
  where
    example = (show . align . vsep) [prettyStgi x :: Doc StgiAnn, "is not equal to", prettyStgi y]
