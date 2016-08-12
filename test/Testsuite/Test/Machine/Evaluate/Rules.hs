{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for each of the STG rules. The scope should be as small as possible
-- around the actual test subject as possible. (To test return-after-case
-- you have to combine case evaluation and return handling, but you do not
-- have to introduce new bindings with @let@, for example.)
module Test.Machine.Evaluate.Rules (tests) where




import Stg.Language
import Stg.Machine
import Stg.Machine.Types
import Stg.Parser.QuasiQuoter (stg)

import           Test.Machine.Evaluate.TestTemplates.MachineState    hiding
    (defSpec)
import qualified Test.Machine.Evaluate.TestTemplates.MachineState    as MachineTest
import qualified Test.Machine.Evaluate.TestTemplates.MarshalledValue as MVal

import Test.Orphans              ()
import Test.QuickCheck.Modifiers
import Test.Tasty



tests :: TestTree
tests = testGroup "Rules"
    [ nonUpdatableFunctionApplication
    , testGroup "Let (rule 3)"
        [ testGroup "Non-recursive"
            [ letBinding
            , letMultiBinding
            , letNestedBinding ]
        , testGroup "Recursive"
            [ letrecBinding
            , letrecMultiBinding ]
        ]
    , testGroup "Case evaluation (rule 4)"
        [ testGroup "Default-only"
            [ defaultOnlyCase_unboundAlgebraic
            , defaultOnlyCase_boundAlgebraic
            , defaultOnlyCase_unboundPrimitive
            , defaultOnlyCase_boundPrimitive ]
        , testGroup "Algebraic alternatives"
            [ algebraicCase_normalMatch
            , algebraicCase_defaultUnboundMatch
            , algebraicCase_defaultBoundMatch ]
        , testGroup "Primitive alternatives"
            [ primitiveCase_normalMatch
            , primitiveCase_defaultUnboundMatch
            , primitiveCase_defaultBoundMatch ]
        ]
    , constructorApplication
    , literalEvaluation
    , literalApplication
    , primops
    , enterUpdatableClosure
    , algebraicReturnUpdate
    , missingArgsUpdate
    , testGroup "Primitive case evaluation shortcuts"
        [ primopShortcut_defaultBound
        , primopShortcut_normalMatch ]
    ]

defSpec :: MachineStateTestSpec
defSpec = MachineTest.defSpec
    { maxSteps  = 32
    , performGc = PerformGc (const Nothing) }

nonUpdatableFunctionApplication :: TestTree
nonUpdatableFunctionApplication = machineStateTest defSpec
    { testName = "Function application, enter non-updatable closure (rule 1 and 2)"
    , source = [stg|
        main = \ => case id unit of
            Unit -> Success;
            default -> TestFail;
        id = \x -> x;
        unit = \ -> Unit
        |] }

letBinding :: TestTree
letBinding = machineStateTest defSpec
    { testName = "Single binding"
    , source = [stg|
        main = \ =>
            let x = \ -> Success
            in x
        |] }

letMultiBinding :: TestTree
letMultiBinding = machineStateTest defSpec
    { testName = "Multiple bindings"
    , source = [stg|
        main = \ =>
            let id = \x -> x;
                one = \ -> Int# 1#
            in case id one of
                Int# y -> case y of
                    1# -> Success;
                    wrong -> TestFail wrong;
                default -> Error
        |] }

letNestedBinding :: TestTree
letNestedBinding = machineStateTest defSpec
    { testName = "Nested bindings"
    , source = [stg|
        main = \ =>
            let id = \x -> x;
                one = \ -> Int# 1#
            in let idOne = \(id one) -> case id one of v -> v
               in case idOne of
                   Int# y -> case y of
                       1# -> Success;
                       wrong -> TestFail wrong;
                   default -> Error
        |] }

letrecBinding :: TestTree
letrecBinding = machineStateTest defSpec
    { testName = "Single binding"
    , source = [stg| main = \ => letrec x = \ -> Success in x |] }

letrecMultiBinding :: TestTree
letrecMultiBinding = machineStateTest defSpec
    { testName = "Cross-referencing bindings"
    , source = [stg|
        main = \ =>
            letrec id = \x -> x;
                   idOne = \(id one) -> case id one of
                       v -> v;
                   one = \ -> Int# 1#
            in case idOne of
                Int# y -> case y of
                    1# -> Success;
                    default -> TestFail;
                default -> Error
        |] }

defaultOnlyCase_unboundAlgebraic :: TestTree
defaultOnlyCase_unboundAlgebraic = machineStateTest defSpec
    { testName = "Unbound, algebraic scrutinee (rule 7)"
    , source = [stg|
        main = \ => case x of
            default -> x;
        x = \ -> Success
        |] }

defaultOnlyCase_boundAlgebraic :: TestTree
defaultOnlyCase_boundAlgebraic = machineStateTest defSpec
    { testName = "Bound, algebraic scrutinee (rule 8)"
    , source = [stg|
        main = \ => case x of
            x -> x;
        x = \ -> Success
        |] }

defaultOnlyCase_unboundPrimitive :: TestTree
defaultOnlyCase_unboundPrimitive = machineStateTest defSpec
    { testName = "Unbound, primitive scrutinee (rule 13)"
    , source = [stg|
        main = \ => case 1# of
            default -> Success
        |] }

defaultOnlyCase_boundPrimitive :: TestTree
defaultOnlyCase_boundPrimitive = machineStateTest defSpec
    { testName = "Bound, primitive scrutinee (rule 12)"
    , source = [stg|
        main = \ => case 1# of
            x -> Success
        |] }

algebraicCase_normalMatch :: TestTree
algebraicCase_normalMatch = machineStateTest defSpec
    { testName = "Algebraic, normal match (rule 6)"
    , source = [stg|
        main = \ => case Nothing of
            Nothing -> Success;
            default -> TestFail
        |] }

algebraicCase_defaultUnboundMatch :: TestTree
algebraicCase_defaultUnboundMatch = machineStateTest defSpec
    { testName = "Algebraic, unbound default match (rule 7)"
    , source = [stg|
        main = \ => case Nothing of
            Just x  -> TestFail x;
            default -> Success
        |] }

algebraicCase_defaultBoundMatch :: TestTree
algebraicCase_defaultBoundMatch = machineStateTest defSpec
    { testName = "Algebraic, bound default match (rule 8)"
    , source = [stg|
        main = \ => case Nothing of
            Just x -> TestFail;
            v      -> Success

        |] }

primitiveCase_normalMatch :: TestTree
primitiveCase_normalMatch = machineStateTest defSpec
    { testName = "Primitive, normal match (rule 11)"
    , source = [stg|
        main = \ => case 1# of
            1#      -> Success;
            default -> TestFail
        |] }

primitiveCase_defaultUnboundMatch :: TestTree
primitiveCase_defaultUnboundMatch = machineStateTest defSpec
    { testName = "Primitive, unbound default match (rule 13)"
    , source = [stg|
        main = \ => case 1# of
            0#      -> TestFail;
            123#    -> TestFail;
            default -> Success
        |] }

primitiveCase_defaultBoundMatch :: TestTree
primitiveCase_defaultBoundMatch = machineStateTest defSpec
    { testName = "Primitive, unbound default match (rule 12)"
    , source = [stg|
        main = \ => case 1# of
            0#   -> TestFail;
            123# -> TestFail;
            -1#  -> TestFail;
            x    -> Success
        |] }

constructorApplication :: TestTree
constructorApplication = machineStateTest defSpec
    { testName = "Constructor application (rule 5)"
    , source = [stg|
        main = \ => case Just 1# of
            Just v -> Success;
            x      -> TestFail x
        |] }

literalEvaluation :: TestTree
literalEvaluation = machineStateTest defSpec
    { testName = "Literal evaluation (rule 9)"
    , source = [stg|
        main = \ => case 1# of
            1# -> Success;
            x  -> TestFail x
        |] }

literalApplication :: TestTree
literalApplication = machineStateTest defSpec
    { testName = "Literal application (rule 10)"
    , source = [stg|
        main = \ => case 1# of
            v1 -> case v1 of
                1# -> Success;
                x  -> TestFail x
        |] }

primops :: TestTree
primops = MVal.marshalledValueTest MVal.MarshalledValueTestSpec
    { MVal.testName = "Primops"
    , MVal.maxSteps = 1024
    , MVal.failWithInfo = False
    , MVal.failPredicate = const False
    , MVal.sourceSpec = \(op, arg1 :: Integer, NonZero arg2) -> MVal.MarshalSourceSpec
            -- arg2 is nonzero or the div/mod tests fail. Having their own tests
            -- is probably not worth the code duplication, so we just throw out
            -- the baby with the bathwater here.
        { MVal.resultVar = "main"
        , MVal.expectedValue = haskell op arg1 arg2
        , MVal.source = Program (Binds
            [(Var "main", LambdaForm [] Update []
                (Case (AppP op (AtomLit (Literal arg1)) (AtomLit (Literal arg2))) (Alts NoNonDefaultAlts
                    (DefaultBound (Var "x") (AppC (Constr "Int#") [AtomVar (Var "x")])) )))])}}
  where
    boolToPrim op x y = if op x y then 1 else 0
    haskell = \case
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> div
        Mod -> mod
        Eq  -> boolToPrim (==)
        Lt  -> boolToPrim (<)
        Leq -> boolToPrim (<=)
        Gt  -> boolToPrim (>)
        Geq -> boolToPrim (>=)
        Neq -> boolToPrim (/=)

enterUpdatableClosure :: TestTree
enterUpdatableClosure = machineStateTest defSpec
    { testName = "Enter updatable closure (rule 15)"
    , source = [stg|
        main = \ => case Unit of
            default -> Success
        |]
    , allSatisfied =
        [ \state -> case stgInfo state of
            Info (StateTransition Rule15_Enter_UpdatableClosure) _ -> True
            _otherwise -> False ]
    }

algebraicReturnUpdate :: TestTree
algebraicReturnUpdate = machineStateTest defSpec
    { testName = "Update because of missing return frame (rule 16)"
    , source = [stg|
        main = \ => case updateMe of
            default -> Success;
        updateMe = \ => case Unit of default -> Unit
        |]
    , allSatisfied =
        [ \state -> case stgInfo state of
            Info (StateTransition Rule16_ReturnCon_Update) _ -> True
            _otherwise -> False ]
    }

missingArgsUpdate :: TestTree
missingArgsUpdate = machineStateTest defSpec
    { testName = "Update because of missing argument frame (rule 17a)"
    , source = [stg|
        main = \ =>
            case flipTuple 1# 2# of
                Tuple a b -> case a of
                    2# -> case b of
                        1# -> Success;
                        bad -> TestFail bad;
                    bad -> TestFail bad;
                badTuple -> Error_badTuple badTuple;
        tuple = \x y -> Tuple x y;
        flip = \f x y -> f y x;
        flipTuple = \ => flip tuple
        |]
    , allSatisfied =
        [ \state -> case stgInfo state of
            Info (StateTransition Rule17a_Enter_PartiallyAppliedUpdate) _ -> True
            _otherwise -> False ]
    }

primopShortcut_defaultBound :: TestTree
primopShortcut_defaultBound = machineStateTest defSpec
    { testName = "Default bound match shortcut (rule 18)"
    , source = [stg|
        main = \ => case +# 1# 2# of
            1# -> TestFail 1#;
            2# -> TestFail 2#;
            v  -> Success
        |]
    , failPredicate = \state -> case stgCode state of
        Eval AppP{} _ -> True -- The point of the shortcut is to never reach
                              -- the AppP rule itself.
        _otherwise    -> False
    }

primopShortcut_normalMatch :: TestTree
primopShortcut_normalMatch = machineStateTest defSpec
    { testName = "Standard match shortcut (rule 19)"
    , source = [stg|
        main = \ => case 1# of
            one -> case +# one 2# of
                3#    -> Success;
                wrong -> TestFail wrong
        |]
    , failPredicate = \state -> case stgCode state of
        Eval AppP{} _ -> True -- The point of the shortcut is to never reach
                              -- the AppP rule itself.
        _otherwise    -> False
    }
