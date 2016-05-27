{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import System.Console.ANSI   (hSupportsANSI)
import System.Console.GetOpt
import System.Environment
import System.IO             (stdout)

import qualified Stg.ExamplePrograms      as Example
import           Stg.Language.Prettyprint
import           Stg.RunForPager



data Options = Options { optAnsi :: Maybe Bool }

defOptions :: Options
defOptions = Options { optAnsi = Nothing }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['c'] ["colour"]
        (OptArg
            (\case Nothing      -> \opts -> opts { optAnsi = Just True  }
                   Just "auto"  -> \opts -> opts { optAnsi = Nothing    }
                   Just "false" -> \opts -> opts { optAnsi = Just False }
                   Just "true"  -> \opts -> opts { optAnsi = Just True  }
                   Just _       -> \opts -> opts { optAnsi = Just False } )
            "auto|false|true" )
        "Colourize output" ]

handleArgs :: [String] -> IO Options
handleArgs argv = case getOpt Permute options argv of
    (o,_,[]) -> pure (foldr id defOptions o)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: $0 [OPTION...]"

main :: IO ()
main = do
    opts <- getArgs >>= handleArgs
    ansi <- case optAnsi opts of
        Just x -> pure x
        Nothing -> hSupportsANSI stdout
    let prog = Example.fibonacciZipWith
    if ansi
        then runForPager prettyprint      prog
        else runForPager prettyprintPlain prog
