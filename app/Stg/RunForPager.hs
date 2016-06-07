{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- | Run a STG program with output suitable for use in a pager, such as @less@.
module Stg.RunForPager (runForPager) where



import           Control.Monad
import           Data.Foldable
import           Data.Monoid
import           Data.Text     (Text)
import qualified Data.Text     as T
import qualified Data.Text.IO  as T

import Stg.Language
import Stg.Language.Prettyprint
import Stg.Machine
import Stg.Machine.Types
import Stg.Util



runForPager
    :: (forall a. Pretty a => a -> Text)
    -> Maybe Int -- ^ Steps to show. Negative numbers count from the end.
    -> Int       -- ^ Verbosity level
    -> Program
    -> IO ()
runForPager ppr showSteps verbosity prog =
    let allStates = evalsUntil RunIndefinitely
                            (HaltIf (const False))
                            (PerformGc (const (Just triStateTracing)))
                            (initialState "main" prog)
        states = case showSteps of
            Just n | n > 0 -> take n allStates
                   | n < 0 -> takeFromEnd (abs n) allStates
            _else -> allStates
        line = T.replicate 80 "-"
        fatLine = T.replicate 80 "="
    in do
        T.putStrLn fatLine
        T.putStrLn "Program:"
        T.putStrLn line
        T.putStrLn (ppr prog)
        for_ states (\state -> do
            T.putStrLn fatLine
            printInfo ppr verbosity state line
            T.putStrLn (ppr state) )
        T.putStrLn fatLine

printInfo
    :: (forall a. Pretty a => a -> Text)
    -> Int
    -> StgState
    -> Text -- ^ Line
    -> IO ()
printInfo ppr verbosity state line =
    when (verbosity > 0)
        (do T.putStr (show' (stgSteps state) <> ". ")
            T.putStrLn
                (if | verbosity == 2 -> ppr (stgInfo state)
                    | verbosity == 1 -> ppr (let Info shortInfo _ = stgInfo state
                                             in shortInfo ))
            T.putStrLn line )

takeFromEnd :: Int -> [a] -> [a]
takeFromEnd n list = zipOverflow (drop n list) list
  where
    zipOverflow (_:xs) (_:ys) = zipOverflow xs ys
    zipOverflow xs [] = xs
    zipOverflow [] ys = ys
