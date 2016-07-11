{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.StaticAnalysis (tests) where



import           Data.Foldable
import           Data.Monoid
import           Data.Set                     (Set)
import qualified Data.Text                    as T
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

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
            , testLit
            ]
        , testShadowGlobal
        ]
    ]

pprFreeVars :: Set Var -> Doc
pprFreeVars = commaSep . map pretty . toList

assertFreeVariablesEqual :: FVars -> FVars -> Assertion
assertFreeVariablesEqual expected@(FVars a b) actual@(FVars x y)
  = assertBool err (expected == actual)
  where
    err = (T.unpack . prettyprintPlain)
        (vsep
            [ hang 4 (vsep
                [ "All free variables:"
                , "Expected: " <> pprFreeVars a
                , "Actual:   " <> pprFreeVars x ])
            , hang 4 (vsep
                [ "Explicitly free variables:"
                , "Expected: " <> pprFreeVars b
                , "Actual:   " <> pprFreeVars y ])])

testLet :: TestTree
testLet = testCase "Simple binding" test
  where
    source = [expr| let f = \(a) b -> a b c in f x |]
    expected = FVars { allFree        = ["a", "c", "x"]
                     , explicitlyFree = ["a", "x"] }
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
    expected = FVars { allFree        = ["f", "x"]
                     , explicitlyFree = ["a", "x"] }
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testCaseAlg :: TestTree
testCaseAlg = testCase "Algebraic" test
  where
    source = [expr| case f x of Con y -> f y; v -> g v |]
    expected = FVars { allFree        = ["f", "g", "x"]
                     , explicitlyFree = ["f", "g", "x"] }
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testCasePrim :: TestTree
testCasePrim = testCase "Primitive" test
  where
    source = [expr| case f x of 1# -> f y; v -> g v |]
    expected = FVars { allFree        = ["f", "g", "x", "y"]
                     , explicitlyFree = ["f", "g", "x", "y"] }
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testCaseDefaultOnly :: TestTree
testCaseDefaultOnly = testCase "Default only" test
  where
    source = [expr| case f x of v -> h v |]
    expected = FVars { allFree        = ["f", "h", "x"]
                     , explicitlyFree = ["f", "h", "x"] }
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testAppF :: TestTree
testAppF = testCase "Function application" test
  where
    source = [expr| f x y |]
    expected = FVars { allFree        = ["f", "x", "y"]
                     , explicitlyFree = ["f", "x", "y"] }
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testCon :: TestTree
testCon = testCase "Constructor application" test
  where
    source = [expr| Con y z |]
    expected = FVars { allFree        = ["y", "z"]
                     , explicitlyFree = ["y", "z"] }
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testPrimop :: TestTree
testPrimop = testCase "Primop" test
  where
    source = [expr| +# p q |]
    expected = FVars { allFree        = ["p", "q"]
                     , explicitlyFree = ["p", "q"] }
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testLit :: TestTree
testLit = testCase "Literal" test
  where
    source = [expr| 1# |]
    expected = FVars { allFree = [], explicitlyFree = [] }
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual

testShadowGlobal :: TestTree
testShadowGlobal = testCase "Shadowing a global" test
  where
    source = [program|
        id = \b -> b;
        main = \ -> let f = \id -> id
                    in f |]
    expected = FVars { allFree = [], explicitlyFree = [] }
    actual = freeVariables source
    test = assertFreeVariablesEqual expected actual
