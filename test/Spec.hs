module Main (main) where



import           Control.Concurrent
import           Data.Monoid
import           Test.Tasty
import           Test.Tasty.Options
import           Test.Tasty.QuickCheck
import           Test.Tasty.Runners

import qualified Test.Language         as Language
import qualified Test.Parser           as Parser
import qualified Test.Stack            as Stack



main :: IO ()
main = do
    options <- testOptions
    defaultMain (options tests)

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
                , option (QuickCheckMaxSize 10) ]

tests :: TestTree
tests = testGroup "STG"
    [ Stack.tests
    , Parser.tests
    , Language.tests ]
