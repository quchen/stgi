{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Prelude.Bool (tests) where



import Data.Bool

import           Stg.Marshal
import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude            as Stg

import Test.Machine.Evaluate.TestTemplates.MarshalledValue
import Test.Orphans                                        ()
import Test.Tasty



tests :: TestTree
tests = testGroup "Bool"
    [ testAnd2
    , testOr2
    , testNot
    , testBool ]

testAnd2 :: TestTree
testAnd2 = marshalledValueTest defSpec
    { testName = "and2 (&&)"
    , failWithInfo = True
    , sourceSpec = \(b1, b2) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = b1 && b2
        , source = mconcat
            [ toStg "b1" b1
            , toStg "b2" b2
            , Stg.and2
            , [stg| main = \ => and2 b1 b2 |] ]}}

testOr2 :: TestTree
testOr2 = marshalledValueTest defSpec
    { testName = "or2 (||)"
    , sourceSpec = \(b1, b2) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = b1 || b2
        , source = mconcat
            [ toStg "b1" b1
            , toStg "b2" b2
            , Stg.or2
            , [stg| main = \ => or2 b1 b2 |] ]}}

testNot :: TestTree
testNot = marshalledValueTest defSpec
    { testName = "not"
    , sourceSpec = \b -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = not b
        , source = mconcat
            [ toStg "b" b
            , Stg.not
            , [stg| main = \ => not b |] ]}}

testBool :: TestTree
testBool = marshalledValueTest defSpec
    { testName = "bool"
    , maxSteps = 1024
    , failPredicate = const False
    , sourceSpec = \(t :: Integer, f, p) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = bool t f p
        , source = mconcat
            [ toStg "p" p
            , toStg "t" t
            , toStg "f" f
            , Stg.bool
            , [stg| main = \ => bool t f p |] ]}}
