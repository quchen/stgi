{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Prelude.Number (tests) where



import Data.Monoid
import Data.Text   (Text)

import           Stg.Language
import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude            as Stg

import Test.Machine.Evaluate.TestTemplates.HaskellReference
import Test.Orphans                                         ()
import Test.QuickCheck.Modifiers
import Test.Tasty



tests :: TestTree
tests = testGroup "Number"
    [ testGroup "Comparison"
        [ testEq
        , testLt
        , testLeq
        , testGt
        , testGeq
        , testNeq ]
    , testGroup "Arithmetic"
        [ testAdd
        , testSub
        , testMul
        , testDiv
        , testMod ]
    , testMin
    , testMax ]

testEq, testLt, testLeq, testGt, testGeq, testNeq :: TestTree
testEq  = testComparison "==" (==) (Stg.eq_Int  <> [stg| stgFunc = \ -> eq_Int  |])
testLt  = testComparison "<"  (<)  (Stg.lt_Int  <> [stg| stgFunc = \ -> lt_Int  |])
testLeq = testComparison "<=" (<=) (Stg.leq_Int <> [stg| stgFunc = \ -> leq_Int |])
testGt  = testComparison ">"  (>)  (Stg.gt_Int  <> [stg| stgFunc = \ -> gt_Int  |])
testGeq = testComparison ">=" (>=) (Stg.geq_Int <> [stg| stgFunc = \ -> geq_Int |])
testNeq = testComparison "/=" (/=) (Stg.neq_Int <> [stg| stgFunc = \ -> neq_Int |])

-- | Test an Integer comparison operator
testComparison
    :: Text -- ^ Test name
    -> (Integer -> Integer -> Bool) -- ^ Comparison operator
    -> Program -- ^ STG definition of the comparison function, named "stgFunc"
    -> TestTree
testComparison name  haskellRef stgFuncDef = haskellReferenceTest defSpec
    { testName = name
    , source = \(x, y) ->
           Stg.toStg "x" x
        <> Stg.toStg "y" y
        <> Stg.toStg "expectedResult" (haskellRef x y)
        <> Stg.eq_Bool
        <> stgFuncDef
        <> [stg|

        main = \ =>
            case stgFunc x y of
                result -> case eq_Bool result expectedResult of
                    True -> Success;
                    False -> TestFail result;
                    badBool -> Error_testMin badBool
        |] }

testAdd, testSub, testMul, testDiv, testMod :: TestTree
testAdd = testArithmetic "+"   (+) (Stg.add  <> [stg| stgFunc = \ -> add |])
testSub = testArithmetic "-"   (-) (Stg.sub  <> [stg| stgFunc = \ -> sub |])
testMul = testArithmetic "*"   (*) (Stg.mul  <> [stg| stgFunc = \ -> mul |])
testDiv = testArithmetic "div" div (Stg.div  <> [stg| stgFunc = \ -> div |])
testMod = testArithmetic "mod" mod (Stg.mod  <> [stg| stgFunc = \ -> mod |])

-- | Test an arithmetic operator
testArithmetic
    :: Text -- ^ Test name
    -> (Integer -> Integer -> Integer) -- ^ Arithmetic operator
    -> Program -- ^ STG definition of the comparison function, named "stgFunc"
    -> TestTree
testArithmetic name  haskellRef stgFuncDef = haskellReferenceTest defSpec
    { testName = name
    , source = \(x, NonZero y) ->
           Stg.toStg "x" x
        <> Stg.toStg "y" y
        <> Stg.toStg "expectedResult" (haskellRef x y)
        <> Stg.eq_Int
        <> stgFuncDef
        <> [stg|

        main = \ =>
            case stgFunc x y of
                result -> case eq_Int result expectedResult of
                    True -> Success;
                    False -> TestFail result;
                    badBool -> Error_testMin badBool
        |] }

testMin :: TestTree
testMin = haskellReferenceTest defSpec
    { testName = "min"
    , source = \(x, y :: Int) ->
           Stg.toStg "x" x
        <> Stg.toStg "y" y
        <> Stg.toStg "expectedResult" (min x y)
        <> Stg.min
        <> Stg.eq_Int
        <> [stg|

        main = \ =>
            case min x y of
                result -> case eq_Int result expectedResult of
                    True -> Success;
                    False -> TestFail result;
                    badBool -> Error_testMin badBool
        |] }

testMax :: TestTree
testMax = haskellReferenceTest defSpec
    { testName = "max"
    , source = \(x, y :: Int) ->
           Stg.toStg "x" x
        <> Stg.toStg "y" y
        <> Stg.toStg "expectedResult" (max x y)
        <> Stg.max
        <> Stg.eq_Int
        <> [stg|

        main = \ =>
            case max x y of
                result -> case eq_Int result expectedResult of
                    True -> Success;
                    False -> TestFail result;
                    badBool -> Error_testMin badBool
        |] }
