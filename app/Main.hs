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
        -- one = () \n () -> 1#;
        -- add = () \n (x, y) -> +# x y;
        -- -- INVALID: three = () \u () -> +# 1# 2#;
        -- -- ... because only variables may be on the RHS ..?
        -- main = () \u () -> add (2#, 1#)

        main = () \u () -> case id (unit) of
            Unit () -> Success ();
            default -> Fail ();
        id = () \n (x) -> x ();
        unit = () \n () -> Unit ()
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
