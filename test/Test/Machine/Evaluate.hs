{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Machine.Evaluate (tests) where



import           Data.Monoid
import           Data.Text
import qualified Data.Text                as T
import           Test.Tasty
import           Test.Tasty.HUnit

import           Stg.Language
import           Stg.Language.Prettyprint
import           Stg.Machine
import           Stg.Machine.Env
import           Stg.Machine.Heap         as H
import           Stg.Machine.Types
import           Stg.Parser
import           Stg.Parser.QuasiQuoter
import           Stg.Util

import           Test.Orphans.Machine     ()


tests :: TestTree
tests = testGroup "Evaluate"
    [ testGroup "Input-output comparisons"
        [ caseMatch ]
    ]

caseMatch :: TestTree
caseMatch = testCase "Case match" test
  where
    source = [stg|
        main = () \u () -> case Nothing () of
            Nothing () -> True ();
            default    -> False ()
        |]
    program = initialState "main" source
    successPredicate = "main" ==> [stgLambdaForm| () \n () -> True () |]
    test = case evalUntil 1e3 successPredicate program of
        Right _ -> pure ()
        Left state -> (assertFailure . T.unpack . T.unlines)
            [ "Machine did not satisfy predicate within " <> show' (stgTicks state) <> "steps."
            , "Final state:"
            , prettyprintAnsi state ]

    var ==> lambdaForm = \state -> case varLookup state var of
        VarLookupClosure (Closure lf _) -> lf == lambdaForm
        _otherwise                      -> False

data VarLookupResult =
      VarLookupError Text
    | VarLookupPrim Integer
    | VarLookupClosure Closure
    deriving (Eq, Ord, Show)

varLookup :: StgState -> Var -> VarLookupResult
varLookup state var =
    case globalVal (stgGlobals state) var of
        Nothing -> VarLookupError "not found in globals"
        Just (Addr addr) -> case H.lookup addr (stgHeap state) of
            Just closure -> VarLookupClosure closure
            Nothing -> VarLookupError "not found on heap"
        Just (PrimInt i) -> VarLookupPrim i
