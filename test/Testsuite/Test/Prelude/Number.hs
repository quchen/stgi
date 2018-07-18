{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Prelude.Number (tests) where



import Data.Text   (Text)

import           Stg.Language
import           Stg.Marshal
import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude            as Stg

import Test.Machine.Evaluate.TestTemplates.MarshalledValue
import Test.Orphans                                        ()
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
testComparison name  haskellRef stgFuncDef = marshalledValueTest defSpec
    { testName = name
    , sourceSpec = \(x, y) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = haskellRef x y
        , source = mconcat
            [ toStg "x" x
            , toStg "y" y
            , stgFuncDef
            , [stg| main = \ => stgFunc x y |] ]}}

testAdd, testSub, testMul, testDiv, testMod :: TestTree
testAdd = testArithmetic "+"   (+) (Stg.add <> [stg| stgFunc = \ -> add |])
testSub = testArithmetic "-"   (-) (Stg.sub <> [stg| stgFunc = \ -> sub |])
testMul = testArithmetic "*"   (*) (Stg.mul <> [stg| stgFunc = \ -> mul |])
testDiv = testArithmetic "div" div (Stg.div <> [stg| stgFunc = \ -> div |])
testMod = testArithmetic "mod" mod (Stg.mod <> [stg| stgFunc = \ -> mod |])

-- | Test an arithmetic operator
testArithmetic
    :: Text -- ^ Test name
    -> (Integer -> Integer -> Integer) -- ^ Arithmetic operator
    -> Program -- ^ STG definition of the comparison function, named "stgFunc"
    -> TestTree
testArithmetic name  haskellRef stgFuncDef = marshalledValueTest defSpec
    { testName = name
    , sourceSpec = \(x, NonZero y) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = haskellRef x y
        , source = mconcat
            [ toStg "x" x
            , toStg "y" y
            , stgFuncDef
            , [stg| main = \ => stgFunc x y |] ]}}

testMin :: TestTree
testMin = marshalledValueTest defSpec
    { testName = "min"
    , sourceSpec = \(x, y :: Integer) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = min x y
        , source = mconcat
            [ toStg "x" x
            , toStg "y" y
            , Stg.min
            , [stg| main = \ => min x y |] ]}}

testMax :: TestTree
testMax = marshalledValueTest defSpec
    { testName = "max"
    , sourceSpec = \(x, y :: Integer) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = max x y
        , source = mconcat
            [ toStg "x" x
            , toStg "y" y
            , Stg.max
            , [stg| main = \ => max x y |] ]}}
