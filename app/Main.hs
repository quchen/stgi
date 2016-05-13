{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where



import Data.Monoid
import System.Console.ANSI   (hSupportsANSI)
import System.Console.GetOpt
import System.Environment
import System.IO             (stdout)

import           Stg.Language
import qualified Stg.Language.Prelude     as Stg
import           Stg.Language.Prettyprint
import           Stg.Parser
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
    if ansi
        then runForPager prettyprintAnsi prog
        else runForPager prettyprint     prog

prog :: Program
prog = mconcat
        [ Stg.add
        , Stg.int "zero" 0
        , Stg.foldl'
        , Stg.zipWith ] <> [stg|

    flipConst = () \n (x, y) -> y ();
    main = () \u () ->
        letrec
            fibo = () \u () ->
                letrec
                    fib0 = (fib1) \n () -> Cons (zero, fib1);
                    fib1 = (fib2)  \u () ->
                        let one = () \n () -> Int# (1#)
                        in Cons (one, fib2);
                    fib2 = (fib0, fib1) \u () -> zipWith (add, fib0, fib1)
                in fib0 ()
        in foldl' (flipConst, zero, fibo)
    |]
