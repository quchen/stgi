{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- | Run a STG program with output suitable for use in a pager, such as @less@.
module Stg.RunForPager (runForPager) where



import           Data.Foldable
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T

import           Stg.Language
import           Stg.Language.Prettyprint
import           Stg.Machine
import           Stg.Machine.Types
import           Stg.Util



runForPager :: (forall a. PrettyAnsi a => a -> Text) -> Program -> IO ()
runForPager ppr prog =
    let states = evalsUntil RunIndefinitely
                            (HaltIf (const False))
                            (PerformGc (const True))
                            (initialState "main" prog)
        line = T.replicate 80 "-"
        fatLine = T.replicate 80 "="
    in do
        T.putStrLn fatLine
        T.putStrLn "Program:"
        T.putStrLn line
        T.putStrLn (ppr prog)
        for_ states (\state -> do
            T.putStrLn fatLine
            T.putStrLn (show' (stgTicks state) <> ". " <> ppr (stgInfo state))
            T.putStrLn line
            T.putStrLn (ppr state) )
        T.putStrLn fatLine
