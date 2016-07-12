module Main (main) where



import System.Console.ANSI (hSupportsANSI)
import System.Environment
import System.IO           (stdout)

import           CmdLineArgs
import qualified Stg.ExamplePrograms      as Example
import           Stg.Language.Prettyprint
import           Stg.RunForPager



main :: IO ()
main = do
    opts <- parseCmdArgs =<< getArgs

    ansi <- case optAnsi opts of
        Just x -> pure x
        Nothing -> hSupportsANSI stdout
    let numStates = optNumStates opts
        verbosity = optVerbosity opts

    let prog = Example.implies True False

    _finalState <-
        runForPager (if ansi then prettyprint else prettyprintPlain)
                    numStates
                    verbosity
                    prog
    pure ()
