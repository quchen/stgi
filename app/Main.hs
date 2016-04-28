{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where



import           Data.Foldable
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           System.Console.ANSI      (hSupportsANSI)
import           System.IO                (stdout)

import           Stg.Language
import qualified Stg.Language.Prelude     as Stg
import           Stg.Language.Prettyprint
import           Stg.Machine
import           Stg.Machine.Types
import           Stg.Parser
import           Stg.Util



main :: IO ()
main = do
    let prog = Stg.listOfNumbers "inputList" (reverse [3,1,2,4])
            <> Stg.listOfNumbers "expectedResult" [1,2,3,4]
            <> Stg.listIntEquals
            <> Stg.sort
            <> [stgProgram|

        main = () \u () ->
            let sorted = () \u () -> sort (inputList)
            in case listIntEquals (expectedResult, sorted) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |]
    ansiSupport <- hSupportsANSI stdout
    if ansiSupport || True
        then runStg prettyprintAnsi prog
        else runStg prettyprint     prog

runStg :: (forall a. PrettyAnsi a => a -> Text) -> Program -> IO ()
runStg ppr prog =
    let states = evalsUntil 1000
                            (HaltIf (const False))
                            (PerformGc (const True))
                            (initialState "main" prog)
        line = T.replicate 80 "-"
        fatLine = T.replicate 80 "="
    in do
        T.putStrLn fatLine
        T.putStrLn "Program:"
        T.putStrLn (ppr prog)
        for_ states (\state -> do
            T.putStrLn fatLine
            T.putStrLn (show' (stgTicks state) <> ". " <> ppr (stgInfo state))
            T.putStrLn line
            T.putStrLn (ppr state) )
        T.putStrLn fatLine
