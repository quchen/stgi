{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where



import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           System.Console.ANSI      (hSupportsANSI)
import           System.IO                (stdout)

import           Stg.Language.Prettyprint
import           Stg.Machine
import           Stg.Machine.Types
import           Stg.Parser
import           Stg.Util



main :: IO ()
main = do
    let prog = [stgProgram|
        main = () \n () -> main ()
        |]

        initial = initialState "main" prog
    ansiSupport <- hSupportsANSI stdout
    if ansiSupport || True
        then loopStg prettyprintAnsi initial
        else loopStg prettyprint     initial

loopStg :: (forall a. PrettyAnsi a => a -> Text) -> StgState -> IO ()
loopStg ppr state = do
    T.putStrLn (T.replicate 80 "=")
    T.putStrLn (show' (stgTicks state) <> ". " <> ppr (stgInfo state))
    let continue = do
            T.putStrLn (T.replicate 80 "-")
            T.putStrLn (ppr state)
            loopStg ppr (evalStep state)
        stop = T.putStrLn (T.replicate 80 "=")
    case stgInfo state of
        Info (StateTransiton{}) _ -> continue
        Info (StateInitial{}) _   -> continue
        _other                    -> stop
