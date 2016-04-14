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
        main = () \u () -> let two = () \u () -> +# 1# 1#
                           in two ()
        |]
        initial = initialState "main" prog
        steps = iterate stgStep initial
    for_ steps (\state -> do
        T.putStrLn (prettyprint state)
        T.putStrLn (T.replicate 80 "=") )
