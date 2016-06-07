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



data Options = Options
    { optAnsi      :: Maybe Bool
    , optNumStates :: Maybe Int
    , optVerbosity :: Int }

defOptions :: Options
defOptions = Options { optAnsi      = Nothing
                     , optNumStates = Nothing
                     , optVerbosity = 1 }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['c'] ["colour"]
        (OptArg
            (\x ->
                let parsed = case x of
                        Nothing      -> Just True
                        Just "auto"  -> Nothing
                        Just "false" -> Just False
                        Just "true"  -> Just True
                        Just _       -> Just False
                in \opts -> opts { optAnsi = parsed } )
            "auto|false|true" )
        "Colourize output"
    , Option ['n'] ["showStates"]
        (OptArg
            (\x ->
                let parsed = case x >>= readMaybe of
                        Just n | n == 0    -> Nothing
                               | otherwise -> Just n
                        Nothing            -> Nothing
                in \opts -> opts { optNumStates = parsed } )
            "int" )
        "Colourize output"
    , Option ['v'] ["verbosity"]
        (OptArg
            (\x ->
                let parsed = case x >>= readMaybe of
                        Just n | 0 <= n && n <= 2 -> n
                        _otherwise                -> 1
                in \opts -> opts { optVerbosity = parsed } )
            "0,1,2" )
        "Verbosity" ]

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
        verbosity = optVerbosity opts

    let prog = Example.sum_foldl' [1..5]

    runForPager (if ansi then prettyprint else prettyprintPlain)
                numStates
                verbosity
                prog
