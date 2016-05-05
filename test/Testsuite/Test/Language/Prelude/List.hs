{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Language.Prelude.List (tests) where



import qualified Data.List                as Reference
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T

import           Stg.Language
import qualified Stg.Language.Prelude     as Stg
import           Stg.Language.Prettyprint
import           Stg.Machine
import           Stg.Machine.Env
import           Stg.Machine.Heap         as H
import           Stg.Machine.Types
import           Stg.Parser
import qualified Stg.Util                 as Util

import           Test.Orphans             ()
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.QuickCheck



tests :: TestTree
tests = testGroup "List"
    [ sort ]


-- | Specifies a test that is based on the reduction of a closure.
data HaskellReferenceTestSpec a = HaskellReferenceTestSpec
    { testName         :: Text
        -- ^ The reference function's name. Used only for display purposes.

    , successPredicate :: StgState -> Bool
        -- ^ Test predicate to determine whether the desired state has been
        -- reached.

    , source           :: a -> Program
        -- ^ STG program to run.

    , maxSteps         :: Integer
        -- ^ Maximum number of steps to take
    }

haskellReferenceTest
    :: (Show a, Arbitrary a)
    => HaskellReferenceTestSpec a
    -> TestTree
haskellReferenceTest testSpec = testProperty (T.unpack (testName testSpec)) test
  where
    test input =
        let program = initialState "main" (source testSpec input)
            finalState = evalUntil (RunForMaxSteps (maxSteps testSpec))
                                   (HaltIf (successPredicate testSpec))
                                   (PerformGc (const False))
                                   program
            failureText = (T.unpack . T.unlines)
                [ "STG version of "
                    <> testName testSpec
                    <> " does not match Haskell's reference implementation."
                , "Failure because: "
                    <> prettyprintAnsi (stgInfo finalState)
                , "Final state:"
                , prettyprintAnsi finalState ]
        in counterexample failureText (case stgInfo finalState of
            Info HaltedByPredicate _ -> True
            _otherwise               -> False )



-- | Build a state predicate that asserts that a certain 'Var' maps to
-- a 'LambdaForm' in the heap.
(===>) :: Var -> LambdaForm -> StgState -> Bool
var ===> lambdaForm = \state -> case varLookup state var of
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
        Util.Failure (NotInScope notInScope) -> VarLookupError (T.intercalate ", " (map (\(Var v) -> v) notInScope) <> " not in global scope")
        Util.Success (Addr addr) -> case H.lookup addr (stgHeap state) of
            Just closure -> VarLookupClosure closure
            Nothing -> VarLookupError "not found on heap"
        Util.Success (PrimInt i) -> VarLookupPrim i



sort :: TestTree
sort = haskellReferenceTest HaskellReferenceTestSpec
    { testName = "sort"
    , successPredicate = "main" ===> [stg| () \n () -> Success () |]
    , source = \xs ->
           Stg.listOfNumbers "inputList" xs
        <> Stg.listOfNumbers "expectedResult" (Reference.sort xs)
        <> Stg.equals_List_Int
        <> Stg.sort
        <> [stgProgram|

        main = () \u () ->
            let sorted = () \u () -> sort (inputList)
            in case equals_List_Int (expectedResult, sorted) of
                True () -> Success ();
                wrong   -> TestFail (wrong)
        |]
    , maxSteps = 1024 }
