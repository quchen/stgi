{-# LANGUAGE OverloadedStrings #-}

module CmdLineArgs (
    parseCmdArgs,
    Options(..),
) where



import System.Console.GetOpt
import Text.Read



data Options = Options
    { optAnsi      :: Maybe Bool
    , optNumStates :: Maybe Int
    , optVerbosity :: Int }

defOptions :: Options
defOptions = Options { optAnsi      = Nothing
                     , optNumStates = Nothing
                     , optVerbosity = defaultVerbosity }

defaultVerbosity :: Int
defaultVerbosity = 2

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
        "Number of states to show. Negative counts from the end."
    , Option ['v'] ["verbosity"]
        (OptArg
            (\x ->
                let parsed = case x >>= readMaybe of
                        Just n | 0 <= n && n <= 2 -> n
                        _otherwise                -> defaultVerbosity
                in \opts -> opts { optVerbosity = parsed } )
            "0,1,2" )
        "Verbosity" ]

parseCmdArgs :: [String] -> IO Options
parseCmdArgs argv = case getOpt Permute options argv of
    (o,_,[]) -> pure (foldr id defOptions o)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: $0 [OPTION...]"
