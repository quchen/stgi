{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Machine.Evaluate.Errors (tests) where



import Test.Tasty

import Stg.Machine.Types
import Stg.Parser.QuasiQuoter

import Test.Machine.Evaluate.TestTemplates.MachineState
import Test.Orphans                                     ()



tests :: TestTree
tests = testGroup "Error conditions"
    [ loopEnterBlackHole
    , functionScrutinee
    , testGroup "Invalid operations"
        [ divisionByZero
        , moduloZero ]
    ]

loopEnterBlackHole :: TestTree
loopEnterBlackHole = machineStateTest defSpec
    { testName = "Loop on entering black hole"
    , source = [stg| main = \ => main |]
    , successPredicate = \state -> case stgInfo state of
        Info (StateError EnterBlackhole) _ -> True
        _otherwise -> False }

functionScrutinee :: TestTree
functionScrutinee = machineStateTest defSpec
    { testName = "Function scrutinee"
    , failWithInfo = False
    , source = [stg|
        id = \x -> x
        main = \ => case id of
            default -> Success
        |]
    , successPredicate = \state -> case stgInfo state of
        Info (StateError NonAlgPrimScrutinee) _ -> True
        _otherwise -> False }

divisionByZero :: TestTree
divisionByZero = machineStateTest defSpec
    { testName = "Division by zero"
    , failWithInfo = False
    , source = [stg|
        main = \ => case /# 1# 0# of
            default -> Failure
        |]
    , successPredicate = \state -> case stgInfo state of
        Info (StateError DivisionByZero) _ -> True
        _otherwise -> False }

moduloZero :: TestTree
moduloZero = machineStateTest defSpec
    { testName = "Modulo by zero"
    , failWithInfo = False
    , source = [stg|
        main = \ => case %# 1# 0# of
            default -> Failure
        |]
    , successPredicate = \state -> case stgInfo state of
        Info (StateError DivisionByZero) _ -> True
        _otherwise -> False }
