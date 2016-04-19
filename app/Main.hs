{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where



import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           System.Console.ANSI      (hSupportsANSI)
import           System.IO                (stdout)

import           Stg.Language.Prettyprint
import           Stg.Machine
import           Stg.Machine.Types
import           Stg.Parser



main :: IO ()
main = do
    let prog = [stg|
        add = () \n (x, y) -> case +# x y of
            v -> Int (v);
        main = () \u () -> add (1#, 2#)
        |]
        initial = initialState "main" prog
    ansiSupport <- hSupportsANSI stdout
    if ansiSupport
        then loopStg prettyprintAnsi initial
        else loopStg prettyprint     initial

loopStg :: (forall a. PrettyAnsi a => a -> Text) -> StgState -> IO ()
loopStg ppr state = do
    T.putStrLn (T.replicate 80 "=")
    T.putStrLn (ppr (stgInfo state))
    case stgInfo state of
        StateTransiton{} -> do
            T.putStrLn (T.replicate 80 "-")
            T.putStrLn (ppr state)
            loopStg ppr (evalStep state)
        _other -> do
            T.putStrLn (T.replicate 80 "=")
