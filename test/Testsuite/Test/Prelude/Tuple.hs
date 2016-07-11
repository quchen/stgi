{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Prelude.Tuple (tests) where



import qualified Data.Tuple as T

import           Stg.Marshal
import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude            as Stg

import Test.Machine.Evaluate.TestTemplates.MarshalledValue
import Test.Orphans                                        ()
import Test.Tasty



tests :: TestTree
tests = testGroup "Tuple"
    [ testFst
    , testSnd
    , testCurry
    , testUncurry
    , testSwap
    ]

testFst :: TestTree
testFst = marshalledValueTest defSpec
    { testName = "fst"
    , sourceSpec = \(tuple :: (Integer, Integer)) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = fst tuple
        , source = mconcat
            [ toStg "tuple" tuple
            , Stg.fst
            , [stg| main = \ => fst tuple |] ]}}

testSnd :: TestTree
testSnd = marshalledValueTest defSpec
    { testName = "snd"
    , sourceSpec = \(tuple :: (Integer, Integer)) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = snd tuple
        , source = mconcat
            [ toStg "tuple" tuple
            , Stg.snd
            , [stg| main = \ => snd tuple |] ]}}

testCurry :: TestTree
testCurry = marshalledValueTest defSpec
    { testName = "curry"
    , sourceSpec = \(x,y :: Integer) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = x+y
        , source = mconcat
            [ toStg "x" x
            , toStg "y" y
            , Stg.curry
            , Stg.add
            , [stg|
            addPair = \tuple -> case tuple of
                Pair a b -> add a b;
                badPair  -> Error_addPair badTuple;

            main = \ => curry addPair x y
            |] ]}}

testUncurry :: TestTree
testUncurry = marshalledValueTest defSpec
    { testName = "uncurry"
    , sourceSpec = \(tuple :: (Integer, Integer)) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = uncurry (+) tuple
        , source = mconcat
            [ toStg "tuple" tuple
            , Stg.uncurry
            , Stg.add
            , [stg| main = \ => uncurry add tuple |] ]}}

testSwap :: TestTree
testSwap = marshalledValueTest defSpec
    { testName = "swap"
    , sourceSpec = \(tuple :: (Integer, Integer)) -> MarshalSourceSpec
        { resultVar = "main"
        , expectedValue = T.swap tuple
        , source = mconcat
            [ toStg "tuple" tuple
            , Stg.swap
            , Stg.equals_Pair_Int
            , [stg| main = \ => swap tuple |] ]}}
