module Main (main) where



import Control.Concurrent
import Data.Monoid

import qualified Test.Language as Language
import qualified Test.Machine  as Machine
import qualified Test.Parser   as Parser
import qualified Test.Stack    as Stack

import Test.Tasty
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Options
import Test.Tasty.QuickCheck
import Test.Tasty.Runners



main :: IO ()
main = do
    options <- testOptions
    defaultMainWithIngredients [ingredients] (options tests)

ingredients :: Ingredient
ingredients = rerunningTests [listingTests, consoleTestReporter]

testOptions :: IO (TestTree -> TestTree)
testOptions = do
    numCapabilities <- getNumCapabilities
    (pure . appEndo . mconcat)
        [ runnerOptions numCapabilities
        , quickcheckOptions]

  where

    option :: IsOption v => v -> Endo TestTree
    option = Endo . localOption

    runnerOptions numThreads = option (NumThreads numThreads)
    quickcheckOptions =
        mconcat [ option (QuickCheckShowReplay False)
                , option (QuickCheckTests 1000)
                , option (QuickCheckMaxSize 5) ]

tests :: TestTree
tests = testGroup "STG"
    [ Stack.tests
    , Parser.tests
    , Machine.tests
    , Language.tests ]
