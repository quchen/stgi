{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Functions and types required to implement tests that check whether
-- a certain closure is reduced to the expected form by the STG.
module Test.Machine.Evaluate.ClosureReductionTest (
    ClosureReductionSpec(..),
    closureReductionTest,
    (==>),
) where


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
import           Stg.Util



-- | Specifies a test that is based on the reduction of a closure.
data ClosureReductionSpec = ClosureReductionSpec
    { testName         :: Text
        -- ^ Test name to display in the test overview.

    , successPredicate :: StgState -> Bool
        -- ^ Test predicate to determine whether the desired state has been
        -- reached.

    , source           :: Program
        -- ^ STG program to run.

    , maxSteps         :: Integer
        -- ^ Maximum number of steps to take

    , performGc        :: PerformGc
    }

-- | Evaluate the @main@ closure of a STG program, and check whether the
-- machine state satisfies a predicate when it is evaluated.
closureReductionTest :: ClosureReductionSpec -> TestTree
closureReductionTest testSpec = testCase (T.unpack (testName testSpec)) test
  where
    program = initialState "main" (source testSpec)
    finalState = evalUntil (maxSteps testSpec)
                           (HaltIf (successPredicate testSpec))
                           (PerformGc (const False))
                           program
    test = case stgInfo finalState of
        Info HaltedByPredicate _ -> pure ()
        _otherwise -> (assertFailure . T.unpack . T.unlines)
            [ "STG failed to satisfy predicate: "
                <> prettyprintAnsi (stgInfo finalState)
            , "Final state:"
            , prettyprintAnsi finalState ]

-- | Build a state predicate that asserts that a certain 'Var' maps to
-- a 'LambdaForm' in the heap.
(==>) :: Var -> LambdaForm -> StgState -> Bool
var ==> lambdaForm = \state -> case varLookup state var of
    VarLookupClosure (Closure lf _) -> lf == lambdaForm
    _otherwise                      -> False

-- | Used as the result of 'varLookup'.
data VarLookupResult =
      VarLookupError Text
    | VarLookupPrim Integer
    | VarLookupClosure Closure
    deriving (Eq, Ord, Show)

-- | Look up the value of a 'Var' on the 'Heap' of a 'StgState'.
varLookup :: StgState -> Var -> VarLookupResult
varLookup state var =
    case globalVal (stgGlobals state) var of
        Failure err -> VarLookupError ("not found in globals - " <> err)
        Success (Addr addr) -> case H.lookup addr (stgHeap state) of
            Just closure -> VarLookupClosure closure
            Nothing -> VarLookupError "not found on heap"
        Success (PrimInt i) -> VarLookupPrim i
