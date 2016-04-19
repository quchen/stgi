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
    let prog = [stgProgram|
        add3 = () \n (x,y,z) -> case x () of
            Int (i) -> case y () of
                Int (j) -> case +# i j of
                    12345# -> 1#; -- type hint FIXME
                    ij -> case z () of
                        Int (k) -> case +# ij k of
                            12345# -> 1#; -- type hint FIXME
                            ijk -> Int (ijk);
                        default -> Error ()
                default -> Error ()
            default -> Error ();

        one   = () \n () -> Int (1#);
        two   = () \n () -> Int (2#);
        three = () \n () -> Int (3#);
        main = () \u () -> case add3 (one, two, three) of
            Int (i) -> case i () of
                6# -> Success ();
                wrongResult -> Fail (wrongResult);
            default -> Error ()
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
