{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- | Run a STG program with output suitable for use in a pager, such as @less@.
module Stg.RunForPager (runForPager, Renderer(..)) where



import           Control.Monad
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T

import Stg.Language
import Stg.Machine
import Stg.Machine.Types
import Stg.Util



data Renderer = Renderer
    { renderProgram   :: Program   -> Text
    , renderState     :: StgState  -> Text
    , renderInfo      :: Info      -> Text
    , renderInfoShort :: InfoShort -> Text
    }

runForPager
    :: Renderer
    -> Maybe Int -- ^ Steps to show. Negative numbers count from the end.
    -> Int       -- ^ Verbosity level
    -> Program
    -> IO StgState
runForPager renderer showSteps verbosity prog =
    let allStates = evalsUntil RunIndefinitely
                               (HaltIf (const False))
                               (PerformGc (const (Just triStateTracing)))
                               (initialState "main" prog)
        states = case showSteps of
            Just n | n > 0 -> NE.fromList (NE.take n allStates)
                   | n < 0 -> unsafeTakeLast (abs n) allStates
            _else -> allStates
        line = T.replicate 80 "-"
        fatLine = T.replicate 80 "="
    in do
        T.putStrLn fatLine
        T.putStrLn "Program:"
        T.putStrLn line
        T.putStrLn (renderProgram renderer prog)
        let loop (state :| rest) = do
                T.putStrLn fatLine
                printInfo renderer verbosity state line
                T.putStrLn (renderState renderer state)
                case rest of
                    [] -> pure state
                    (s:ss) -> loop (s:|ss)
        finalState <- loop states
        T.putStrLn fatLine
        pure finalState


printInfo
    :: Renderer
    -> Int
    -> StgState
    -> Text -- ^ Line
    -> IO ()
printInfo renderer verbosity state line =
    when (verbosity > 0)
        (do T.putStr (show' (stgSteps state) <> ". ")
            T.putStrLn
                (if | verbosity == 2 -> renderInfo renderer (stgInfo state)
                    | verbosity == 1 -> renderInfoShort renderer (let Info shortInfo _ = stgInfo state
                                                                  in shortInfo ))
            T.putStrLn line )

-- | Take the last N elements of a list (in original order).
--
-- Number of takes must be at least 1!
unsafeTakeLast :: Int -> NonEmpty a -> NonEmpty a
unsafeTakeLast n _ | n <= 0 = error "unsafeTakeLast: argument must be >= 1"
unsafeTakeLast n list
  = let list' = NE.toList list
    in NE.fromList (zipOverflow (drop n list') list')
  where
    zipOverflow (_:xs) (_:ys) = zipOverflow xs ys
    zipOverflow xs [] = xs
    zipOverflow [] ys = ys
