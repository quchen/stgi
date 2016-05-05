{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Language.Prelude.Number (tests) where



import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T

import           Stg.Language
import qualified Stg.Language.Prelude     as Stg
import           Stg.Language.Prettyprint
import           Stg.Machine
import           Stg.Machine.Env
import           Stg.Machine.Heap         as H
import           Stg.Machine.Types
import           Stg.Parser
import qualified Stg.Util                 as Util

import           Test.Orphans             ()
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.QuickCheck



tests :: TestTree
tests = testGroup "Number"
    [  ]
