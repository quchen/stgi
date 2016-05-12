{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where


import Data.Monoid
import System.Console.ANSI (hSupportsANSI)
import System.IO           (stdout)

import           Stg.Language
import qualified Stg.Language.Prelude     as Stg
import           Stg.Language.Prettyprint
import           Stg.Parser
import           Stg.RunForPager



main :: IO ()
main = do
    ansiSupport <- hSupportsANSI stdout
    if ansiSupport || True
        then runForPager prettyprintAnsi prog
        else runForPager prettyprint     prog

prog :: Program
prog = mconcat
        [ Stg.seq
        , Stg.add
        , Stg.int "zero" 0
        , Stg.foldl'
        , Stg.zipWith ] <> [stgProgram|

    flipConst = () \n (x, y) -> y ();
    main = () \u () ->
        letrec
            fibo = () \u () ->
                letrec
                    fib0 = (fib1) \u () -> Cons (zero, fib1);
                    fib1 = (fib2)  \u () ->
                        let one = () \n () -> Int# (1#)
                        in Cons (one, fib2);
                    fib2 = (fib0, fib1) \u () -> zipWith (add, fib0, fib1)
                in fib0 ()
        in foldl' (flipConst, zero, fibo)
    |]
