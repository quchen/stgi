{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
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
    let prog =     Stg.equals_List_Int
                <> Stg.int "zero" 0
                <> Stg.int "one" 1
                <> Stg.add
                <> Stg.foldl'
                <> Stg.take
                <> Stg.zipWith
                <> [stgProgram|

        sum = () \u () -> foldl' (add, zero);
        main = () \u () ->
            letrec
                fibos = (fibo) \n () -> take (numFibos, fibo);
                fibo = () \u () ->
                    letrec
                        fib0 = (fib1) \u () -> Cons (zero, fib1);
                        fib1 = (fib2) \u () -> Cons (one, fib2);
                        fib2 = (fib0, fib1) \u () -> zipWith (add, fib0, fib1)
                    in fib0 ()
            in sum (fibo)
        |]
    ansiSupport <- hSupportsANSI stdout
    if ansiSupport || True
        then runStg prettyprintAnsi prog
        else runStg prettyprint     prog

runStg :: (forall a. PrettyAnsi a => a -> Text) -> Program -> IO ()
runStg ppr prog =
    let states = evalsUntil RunIndefinitely
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
