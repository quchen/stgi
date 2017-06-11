{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.StaticAnalysis (tests) where



import           Data.Foldable
import           Data.Set                  (Set)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc

import Stg.Language
import Stg.Language.Prettyprint
import Stg.Parser.QuasiQuoter
import Stg.StaticAnalysis
import Stg.Util

import Test.Orphans     ()
import Test.Tasty
import Test.Tasty.HUnit

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}



tests :: TestTree
tests = testGroup "Static analysis"
    [ testGroup "Free variables"
        [ testGroup "Expr"
            [ testGroup "Let"
                [ testLet
                , testLetStress ]
            , testGroup "Case"
                [ testCaseAlg
                , testCasePrim ]
                , testCaseDefaultOnly
            , testAppF
            , testCon
            , testPrimop
            , testLitE
            ]
        , testShadowGlobal
        ]
    ]

pprFreeVars :: Set Var -> Doc StgiAnn
pprFreeVars = commaSep . map prettyStgi . toList

assertFreeVariablesEqual :: Set Var -> Set Var -> Assertion
assertFreeVariablesEqual expected actual
  = assertBool err (expected == actual)
  where
    err = (T.unpack . renderPlain . hang 4 . vsep)
        [ "Free variables:"
        , "Expected: " <> pprFreeVars expected
        , "Actual:   " <> pprFreeVars actual ]

testLet :: TestTree
testLet = testCase "Simple binding" test
  where
    source = [expr| let f = \(a) b -> a b c in f x |]
    expected =  ["a", "x"]
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testLetStress :: TestTree
testLetStress = testCase "Aliasing" test
  where
    source = [expr| let a = \ -> XXX;
                        f = \(a) b ->
                            let g = \(b) -> b x;
                                h = \ -> f
                            in g h
                    in f x |]
    expected = ["a", "x"]
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testCaseAlg :: TestTree
testCaseAlg = testCase "Algebraic" test
  where
    source = [expr| case f x of Con y -> f y; v -> g v |]
    expected =  ["f", "g", "x"]
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testCasePrim :: TestTree
testCasePrim = testCase "Primitive" test
  where
    source = [expr| case f x of 1# -> f y; v -> g v |]
    expected = ["f", "g", "x", "y"]
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testCaseDefaultOnly :: TestTree
testCaseDefaultOnly = testCase "Default only" test
  where
    source = [expr| case f x of v -> h v |]
    expected =  ["f", "h", "x"]
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testAppF :: TestTree
testAppF = testCase "Function application" test
  where
    source = [expr| f x y |]
    expected = ["f", "x", "y"]
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testCon :: TestTree
testCon = testCase "Constructor application" test
  where
    source = [expr| Con y z |]
    expected =  ["y", "z"]
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testPrimop :: TestTree
testPrimop = testCase "Primop" test
  where
    source = [expr| +# p q |]
    expected =  ["p", "q"]
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testLitE :: TestTree
testLitE = testCase "Literal" test
  where
    source = [expr| 1# |]
    expected =   []
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testShadowGlobal :: TestTree
testShadowGlobal = testCase "Shadowing a global" test
  where
    source = [program|
        id = \b -> b;
        main = \ -> let f = \id -> id
                    in f |]
    expected =  []
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual
