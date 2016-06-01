{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import System.Console.ANSI   (hSupportsANSI)
import System.Console.GetOpt
import System.Environment
import System.IO             (stdout)
import Text.Read

import qualified Stg.ExamplePrograms      as Example
import           Stg.Language.Prettyprint
import           Stg.RunForPager



data Options = Options { optAnsi       :: Maybe Bool
                       , optNumStates :: Maybe Int }

defOptions :: Options
defOptions = Options { optAnsi       = Nothing
                     , optNumStates = Nothing }

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
        "Colourize output"
    , Option ['n'] ["showStates"]
        (OptArg
            (\x -> case x >>= readMaybe of
                   Just n | n == 0    -> \opts -> opts { optNumStates = Nothing }
                          | otherwise -> \opts -> opts { optNumStates = Just n  }
                   _otherwise         -> \opts -> opts { optNumStates = Nothing } )
            "int" )
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
    let numStates = optNumStates opts

    let prog = Example.sum_foldl' [1..5]
    if ansi
        then runForPager prettyprint      numStates prog
        else runForPager prettyprintPlain numStates prog
