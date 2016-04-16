{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Machine.Evaluate (tests) where



import           Data.Text
import           Test.Tasty
import           Test.Tasty.HUnit

import           Stg.Language
import           Stg.Machine
import           Stg.Machine.Env
import           Stg.Machine.Heap       as H
import           Stg.Machine.Types
import           Stg.Parser
import           Stg.Parser.Parser
import           Stg.Parser.QuasiQuoter

import           Test.Orphans.Machine   ()


tests :: TestTree
tests = testGroup "Evaluate"
    [ testGroup "Input-output comparisons"
        [ caseMatch ]
    ]

caseMatch :: TestTree
caseMatch = testCase "Case match" test
  where
    source = [stg|
        main = () \u () -> case Just () of
            Just () -> Yes ();
            Nothing () -> Maybe ();
            default -> No ()
        |]
    program = initialState "main" source
    test =
        case evalUntil 1e3 (const True) program of
            Left state -> assertFailure (Prelude.unwords [ "Machine did not halt in"
                                                 , show (stgTicks state)
                                                 , "steps" ])
            Right state -> assertBool "Case test description not implemented"
                (globalVal (stgGlobals state) "main"
                ==
                Just (undefined [stgLambdaForm| () \n () -> Unit () |]) )

heapLookup :: StgState -> Var -> Either Text (Either Integer Closure)
heapLookup state var =
    case globalVal (stgGlobals state) var of
        Nothing -> Left "not found in globals"
        Just (Addr addr) -> case H.lookup addr (stgHeap state) of
            Just x -> Right (Right x)
            Nothing -> Left "not found on heap"
        Just (PrimInt i) -> Right (Left i)
