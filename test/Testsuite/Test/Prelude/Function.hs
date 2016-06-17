{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Prelude.Function (tests) where



import Data.Function

import           Stg.Marshal
import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude            as Stg

import Test.Machine.Evaluate.TestTemplates.MarshalledValue
import Test.Orphans                                        ()
import Test.QuickCheck.Modifiers
import Test.Tasty



tests :: TestTree
tests = testGroup "Function"
    [ testId
    , testConst
    , testCompose
    , testFix ]

testId :: TestTree
testId = marshalledValueTest defSpec
    { testName = "id"
    , sourceSpec = \(x :: Integer) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = x
        , source = mconcat
            [ toStg "x" x
            , Stg.id
            , [stg| main = \ => id x |] ]}}

testConst :: TestTree
testConst = marshalledValueTest defSpec
    { testName = "const"
    , sourceSpec = \(x :: Integer, y :: Integer) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = x
        , source = mconcat
            [ toStg "x" x
            , toStg "y" y
            , Stg.const
            , [stg| main = \ => const x y |] ]}}


testCompose :: TestTree
testCompose = marshalledValueTest defSpec
    { testName = "compose (.)"
    , sourceSpec = \x -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = ((*3) . (+2)) x
        , source = mconcat
            [ toStg "x"     (x :: Integer)
            , toStg "two"   (2 :: Integer)
            , toStg "three" (3 :: Integer)
            , Stg.add
            , Stg.mul
            , Stg.compose
            , Stg.const
            , [stg|
            plus2 = \x -> add x two;
            times3 = \x -> mul x three;
            plus2times3 = \ -> compose times3 plus2;
            main = \ => plus2times3 x |] ]}}

testFix :: TestTree
testFix = marshalledValueTest defSpec
    { testName = "fix"
    , sourceSpec = \(NonNegative (n :: Integer)) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue =
            let fac' = \rec m -> if m == 0 then 1 else m * rec (m-1)
                fac = fix fac'
            in fac n
        , source = mconcat
            [ toStg "n" n
            , toStg "zero" (0 :: Integer)
            , toStg "one" (1 :: Integer)
            , Stg.sub
            , Stg.mul
            , Stg.fix
            , Stg.leq_Int
            , Stg.const
            , [stg|
            fac' = \rec m -> case leq_Int m zero of
                True -> one;
                False -> case sub m one of
                    mMinusOne -> case rec mMinusOne of
                        recMMinusOne -> mul m recMMinusOne;
                badBool -> Error_fac' badBool;

            fac = \ => fix fac';

            main = \ => fac n |] ]}}
