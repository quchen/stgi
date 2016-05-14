{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Language.Prelude.Number (tests) where



import Data.Monoid
import Data.Text   (Text)

import           Stg.Language
import qualified Stg.Language.Prelude as Stg
import           Stg.Parser

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
testEq  = testComparison "==" (==) (Stg.eq_Int  <> [stg| stgFunc = () \n () -> eq_Int  () |])
testLt  = testComparison "<"  (<)  (Stg.lt_Int  <> [stg| stgFunc = () \n () -> lt_Int  () |])
testLeq = testComparison "<=" (<=) (Stg.leq_Int <> [stg| stgFunc = () \n () -> leq_Int () |])
testGt  = testComparison ">"  (>)  (Stg.gt_Int  <> [stg| stgFunc = () \n () -> gt_Int  () |])
testGeq = testComparison ">=" (>=) (Stg.geq_Int <> [stg| stgFunc = () \n () -> geq_Int () |])
testNeq = testComparison "/=" (/=) (Stg.neq_Int <> [stg| stgFunc = () \n () -> neq_Int () |])

-- | Test an Integer comparison operator
testComparison
    :: Text -- ^ Test name
    -> (Integer -> Integer -> Bool) -- ^ Comparison operator
    -> Program -- ^ STG definition of the comparison function, named "stgFunc"
    -> TestTree
testComparison name  haskellRef stgFuncDef = haskellReferenceTest defSpec
    { testName = name
    , source = \(x, y) ->
           Stg.int "x" x
        <> Stg.int "y" y
        <> Stg.boolValue "expectedResult" (haskellRef x y)
        <> Stg.eq_Bool
        <> stgFuncDef
        <> [stg|

        main = () \u () ->
            case stgFunc (x,y) of
                result -> case eq_Bool (result, expectedResult) of
                    True () -> Success ();
                    False () -> TestFail (result);
                    badBool -> Error_testMin (badBool)
        |] }

testAdd, testSub, testMul, testDiv, testMod :: TestTree
testAdd = testArithmetic "+"   (+) (Stg.add  <> [stg| stgFunc = () \n () -> add () |])
testSub = testArithmetic "-"   (-) (Stg.sub  <> [stg| stgFunc = () \n () -> sub () |])
testMul = testArithmetic "*"   (*) (Stg.mul  <> [stg| stgFunc = () \n () -> mul () |])
testDiv = testArithmetic "div" div (Stg.div  <> [stg| stgFunc = () \n () -> div () |])
testMod = testArithmetic "mod" mod (Stg.mod  <> [stg| stgFunc = () \n () -> mod () |])

-- | Test an arithmetic operator
testArithmetic
    :: Text -- ^ Test name
    -> (Integer -> Integer -> Integer) -- ^ Arithmetic operator
    -> Program -- ^ STG definition of the comparison function, named "stgFunc"
    -> TestTree
testArithmetic name  haskellRef stgFuncDef = haskellReferenceTest defSpec
    { testName = name
    , source = \(x, NonZero y) ->
           Stg.int "x" x
        <> Stg.int "y" y
        <> Stg.int "expectedResult" (haskellRef x y)
        <> Stg.eq_Int
        <> stgFuncDef
        <> [stg|

        main = () \u () ->
            case stgFunc (x,y) of
                result -> case eq_Int (result, expectedResult) of
                    True () -> Success ();
                    False () -> TestFail (result);
                    badBool -> Error_testMin (badBool)
        |] }

testMin :: TestTree
testMin = haskellReferenceTest defSpec
    { testName = "min"
    , source = \(x, y) ->
           Stg.int "x" x
        <> Stg.int "y" y
        <> Stg.int "expectedResult" (min x y)
        <> Stg.min
        <> Stg.eq_Int
        <> [stg|

        main = () \u () ->
            case min (x,y) of
                result -> case eq_Int (result, expectedResult) of
                    True () -> Success ();
                    False () -> TestFail (result);
                    badBool -> Error_testMin (badBool)
        |] }

testMax :: TestTree
testMax = haskellReferenceTest defSpec
    { testName = "max"
    , source = \(x, y) ->
           Stg.int "x" x
        <> Stg.int "y" y
        <> Stg.int "expectedResult" (max x y)
        <> Stg.max
        <> Stg.eq_Int
        <> [stg|

        main = () \u () ->
            case max (x,y) of
                result -> case eq_Int (result, expectedResult) of
                    True () -> Success ();
                    False () -> TestFail (result);
                    badBool -> Error_testMin (badBool)
        |] }
