module Main (main) where



import Control.Concurrent
import Data.Monoid

import Test.Tasty
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Options
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import Test.Tasty.Runners.Html

import qualified Test.Language       as Language
import qualified Test.Machine        as Machine
import qualified Test.Marshal        as Marshal
import qualified Test.Parser         as Parser
import qualified Test.Prelude        as Prelude
import qualified Test.Stack          as Stack
import qualified Test.StaticAnalysis as StaticAnalysis



main :: IO ()
main = do
    options <- testOptions
    defaultMainWithIngredients ingredients (options tests)

ingredients :: [Ingredient]
ingredients = [rerunningTests (htmlRunner : defaultIngredients)]

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
    [ Language.tests
    , Machine.tests
    , Marshal.tests
    , Parser.tests
    , Prelude.tests
    , Stack.tests
    , StaticAnalysis.tests ]
