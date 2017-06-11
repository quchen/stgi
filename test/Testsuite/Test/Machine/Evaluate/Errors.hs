{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Machine.Evaluate.Errors (tests) where



import Test.Tasty

import Stg.Language
import Stg.Machine.Types
import Stg.Parser.QuasiQuoter

import Test.Machine.Evaluate.TestTemplates.MachineState
import Test.Orphans                                     ()



tests :: TestTree
tests = testGroup "Error conditions"
    [ updatableClosureWithArgs
    , returnIntWithEmptyReturnStack
    , updateClosureWithPrimitive
    , functionArgumentNotInScope
    , constructorArgumentNotInScope
    , primitiveArgumentNotInScope
    , algebraicReturnToPrimitiveAlts
    , primReturnToAlgAlts
    , loopEnterBlackHole
    , functionScrutinee
    , testGroup "Invalid operations"
        [ divisionByZero
        , moduloZero ]
    , badConArity
    ]

updatableClosureWithArgs :: TestTree
updatableClosureWithArgs = machineStateTest defSpec
    { testName = "Updatable closure with arguments"
    , source = Program (Binds
        [(Var "main",
            LambdaForm [] Update [Var "x"] (AppF (Var "x") []) )])
    , successPredicate = \state -> case stgInfo state of
        Info (StateError UpdatableClosureWithArgs) _ -> True
        _otherwise -> False }

returnIntWithEmptyReturnStack :: TestTree
returnIntWithEmptyReturnStack = machineStateTest defSpec
    { testName = "Closure with primitive body"
    , source = [stg| main = \ -> case 1# of v -> v |]
    , successPredicate = \state -> case stgInfo state of
        Info (StateError ReturnIntWithEmptyReturnStack) _ -> True
        _otherwise -> False }

updateClosureWithPrimitive :: TestTree
updateClosureWithPrimitive = machineStateTest defSpec
    { testName = "Closure update with primitive"
    , source = [stg| main = \ => case 1# of v -> v |]
    , successPredicate = \state -> case stgInfo state of
        Info (StateError UpdateClosureWithPrimitive) _ -> True
        _otherwise -> False }

functionArgumentNotInScope :: TestTree
functionArgumentNotInScope = machineStateTest defSpec
    { testName = "Function argument not in scope"
    , source = [stg| main = \ -> main x |]
    , successPredicate = \state -> case stgInfo state of
        Info (StateError VariablesNotInScope{}) _ -> True
        _otherwise -> False }

constructorArgumentNotInScope :: TestTree
constructorArgumentNotInScope = machineStateTest defSpec
    { testName = "Function argument not in scope"
    , source = [stg| main = \ -> Con x |]
    , successPredicate = \state -> case stgInfo state of
        Info (StateError VariablesNotInScope{}) _ -> True
        _otherwise -> False }

primitiveArgumentNotInScope :: TestTree
primitiveArgumentNotInScope = machineStateTest defSpec
    { testName = "Primitive function argument not in scope"
    , source = [stg| main = \ -> case +# x y of default -> Foo |]
    , successPredicate = \state -> case stgInfo state of
        Info (StateError VariablesNotInScope{}) _ -> True
        _otherwise -> False }

algebraicReturnToPrimitiveAlts :: TestTree
algebraicReturnToPrimitiveAlts = machineStateTest defSpec
    { testName = "Algebraic scrutinee with primitive alts"
    , source = [stg| main = \ -> case Con of 1# -> A; default -> B |]
    , successPredicate = \state -> case stgInfo state of
        Info (StateError AlgReturnToPrimAlts) _ -> True
        _otherwise -> False }

primReturnToAlgAlts :: TestTree
primReturnToAlgAlts = machineStateTest defSpec
    { testName = "Primitive scrutinee with algebraic alts"
    , source = [stg| main = \ -> case 1# of Con -> A; default -> B |]
    , successPredicate = \state -> case stgInfo state of
        Info (StateError PrimReturnToAlgAlts) _ -> True
        _otherwise -> False }

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
        id = \x -> x;
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

badConArity :: TestTree
badConArity = machineStateTest defSpec
    { testName = "Bad constructor arity"
    , failWithInfo = False
    , source = [stg|
        x = \ -> Unit;
        y = \ -> Unit;
        main = \ => case Cons x y of
            Cons x -> Bad;
            default -> Baaad
        |]
    , successPredicate = \state -> case stgInfo state of
        Info (StateError BadConArity{}) _ -> True
        _otherwise -> False }
