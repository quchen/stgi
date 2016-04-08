module Main (main) where

import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Test.Parser           as Parser
import qualified Test.Stack            as Stack

main :: IO ()
main = defaultMain (options tests)

options :: TestTree -> TestTree
options = quickcheckOptions
  where
    quickcheckOptions =
          localOption (QuickCheckShowReplay False)
        . localOption (QuickCheckTests 1000)
        . localOption (QuickCheckMaxSize 100)

tests :: TestTree
tests = testGroup "STG"
    [ Stack.tests
    , Parser.tests ]
