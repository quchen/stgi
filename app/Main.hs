{-# LANGUAGE QuasiQuotes #-}

module Main (main) where



import           Data.Foldable
import qualified Data.Text                as T
import qualified Data.Text.IO             as T

import           Stg.Language.Prettyprint
import           Stg.Machine
import           Stg.Parser



main :: IO ()
main = do
    let prog = [stg|
        one = () \n () -> 1#;
        add = () \n (x, y) -> +# x y;
        -- INVALID: three = () \u () -> +# 1# 2#;
        -- ... because only variables may be on the RHS ..?
        main = () \u () -> add (one, one)
        |]
        initial = initialState "main" prog
        steps = iterate stgStep initial
    for_ steps (\state -> do
        T.putStrLn (prettyprint state)
        T.putStrLn (T.replicate 80 "=") )
