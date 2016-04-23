{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Machine.Evaluate (tests) where

-- TODO: Important tests to add:
--   - Only case does evaluation
--   - Don't forget to add the variables closed over in let(rec)
--   - Primops can only be applied to primitive ints, not int-valued variables.
--     For example, `add2 = () \n (x,y) -> +# x y` is wrong: unboxing-boxing
--     needs to be done.



import           Test.Tasty

import qualified Test.Machine.Evaluate.Medium as Medium
import qualified Test.Machine.Evaluate.Small  as Small
import           Test.Orphans                 ()



tests :: TestTree
tests = testGroup "Evaluate" [Small.tests, Medium.tests]
