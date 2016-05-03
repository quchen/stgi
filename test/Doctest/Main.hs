module Main (main) where


import           Control.Monad
import           System.Environment

import           Test.DocTest



main :: IO ()
main = do
    args <- getArgs
    when (null args) (doctest ["src"])
