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

import qualified Stg.Language.Prelude     as Stg
import           Stg.Language.Prettyprint
import           Stg.Machine
import           Stg.Machine.Types
import           Stg.Parser
import           Stg.Util



main :: IO ()
main = do
    let prog = Stg.foldl' <> Stg.add <> Stg.listC <> [stgProgram|
        zero = () \n () -> Int# (0#);

        sum = () \n (xs) -> foldl' (add, zero, xs);

        list = () \u () ->
            letrec one   = () \n () -> Int# (1#);
                   two   = () \n () -> Int# (2#);
                   three = () \n () -> Int# (3#);
                   list3    = (three)          \n () -> cons (three, nil);
                   list23   = (two, list3)     \n () -> cons (two,   list3);
                   list123  = (one, list23)    \n () -> cons (one,   list23);
                   list3123 = (three, list123) \n () -> cons (three, list123)
            in list3123 ();

        main = () \u () -> case sum (list) of
            Int# (i) -> case i () of
                9# -> Success ();
                wrongResult -> TestFail (wrongResult);
            default -> Error ()
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
