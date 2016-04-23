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

import qualified Stg.Language.Prelude     as Stg
import           Stg.Language.Prettyprint
import           Stg.Machine
import           Stg.Machine.Types
import           Stg.Parser
import           Stg.Util



main :: IO ()
main = do
    let prog = Stg.add <> [stg| main = () \n () -> main () |]
        initial = initialState "main" prog
    ansiSupport <- hSupportsANSI stdout
    if ansiSupport || True
        then runStg prettyprintAnsi initial
        else runStg prettyprint     initial

runStg :: (forall a. PrettyAnsi a => a -> Text) -> StgState -> IO ()
runStg ppr initial =
    let states = evalsUntil 1000
                            (HaltIf (const False))
                            (PerformGc (const True))
                            initial
    in do
        for_ states (\state -> do
            T.putStrLn (T.replicate 80 "=")
            T.putStrLn (show' (stgTicks state) <> ". " <> ppr (stgInfo state))
            T.putStrLn (T.replicate 80 "-")
            T.putStrLn (ppr state) )
        T.putStrLn (T.replicate 80 "=")
