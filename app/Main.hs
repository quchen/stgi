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
    let prog = Stg.int "two" 2
            <> Stg.take
            <> Stg.repeat
            <> Stg.foldr
            <> Stg.seq
            <> [stgProgram|

        consBang = () \n (x,xs) -> case xs () of v -> Cons (x, v);
        nil = () \n () -> Nil ();
        forceSpine = () \n (xs) -> foldr (consBang, nil, xs);

        twoUnits = () \u () ->
            letrec  repeated = (unit) \u () -> repeat (unit);
                    unit = () \n () -> Unit ();
                    take2 = (repeated) \u () -> take (two, repeated)
            in      forceSpine (take2);

        main = () \u () -> case twoUnits () of
            Cons (x,xs) -> case xs () of
                Cons (y,ys) -> case ys () of
                    Nil () -> Success ();
                    default -> TestFailure ();
                default -> TestFailure ();
            default -> TestFailure ()
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
