{-# LANGUAGE OverloadedLists #-}

-- | STG error transitions, in order to provide useful information to the user.
module Stg.Machine.Evaluate.ErrorTransitions (
    updatableClosureWithArgs,
    returnWithEmptyReturnStack,
    functionArgumentNotInScope,
    constructorArgumentNotInScope,
    primopArgumentNotInScope,
    algReturnToPrimAlts,
    primReturnToAlgAlts,
    enterBlackhole,
    updateClosureWithPrimitive,
    nonAlgPrimScrutinee,
    divisionByZero,
    badConArity,
) where



import           Data.Stack                  (Stack (..))
import           Stg.Language
import           Stg.Machine.Env
import           Stg.Machine.Evaluate.Common
import qualified Stg.Machine.Heap            as H
import           Stg.Machine.Types
import           Stg.Util



-- | Page 39, 2nd paragraph: "[...] closures with non-empty argument lists are
-- never updatable [...]"
updatableClosureWithArgs :: StgState -> Maybe StgState
updatableClosureWithArgs s@StgState
    { stgCode = Enter addr
    , stgHeap = heap }
    | Just (HClosure (Closure (LambdaForm _ Update (_:_) _) _))
        <- H.lookup addr heap

  = Just (s { stgInfo = Info (StateError UpdatableClosureWithArgs) [] })

updatableClosureWithArgs _ = Nothing



-- | Page 39, 4th paragraph: "It is not possible for the ReturnInt state to see an
-- empty return stack, because that would imply that a closure should be updated
-- with a primitive value; but no closure has a primitive type."
returnWithEmptyReturnStack :: StgState -> Maybe StgState
returnWithEmptyReturnStack s@StgState
    { stgCode  = ReturnInt{}
    , stgStack = Empty }

  = Just (s { stgInfo = Info (StateError ReturnIntWithEmptyReturnStack)
                             [Detail_ReturnIntCannotUpdate] })

returnWithEmptyReturnStack _ = Nothing



-- | A function was applied to an argument that was neither globally defined
-- nor in the local environment
functionArgumentNotInScope :: StgState -> Maybe StgState
functionArgumentNotInScope s@StgState
    { stgCode    = Eval (AppF f xs) locals
    , stgGlobals = globals }
    | Failure notInScope <- vals locals globals (AtomVar f : xs)

  = Just (s { stgInfo = Info (StateError (VariablesNotInScope notInScope)) [] })

functionArgumentNotInScope _ = Nothing



-- | A constructor was applied to an argument that was neither globally defined
-- nor in the local environment
constructorArgumentNotInScope :: StgState -> Maybe StgState
constructorArgumentNotInScope s@StgState
    { stgCode    = Eval (AppC _con xs) locals
    , stgGlobals = globals }
    | Failure notInScope <- vals locals globals xs

  = Just (s { stgInfo = Info (StateError (VariablesNotInScope notInScope)) [] })

constructorArgumentNotInScope _ = Nothing



-- | A primitive operation was applied to an argument that was neither globally
-- defined nor in the local environment
primopArgumentNotInScope :: StgState -> Maybe StgState
primopArgumentNotInScope s@StgState
    { stgCode = Eval (AppP _op x y) locals }
    | Failure notInScope <- traverse (localVal locals) ([x,y] :: [Atom])

  = Just (s { stgInfo = Info (StateError (VariablesNotInScope notInScope)) [] })

primopArgumentNotInScope _ = Nothing



-- | Algebraic constructor return, but primitive alternative on return frame
algReturnToPrimAlts :: StgState -> Maybe StgState
algReturnToPrimAlts s@StgState
    { stgCode  = ReturnCon{}
    , stgStack = ReturnFrame (Alts PrimitiveAlts{} _) _ :< _ }

  = Just (s { stgInfo = Info (StateError AlgReturnToPrimAlts) [] })

algReturnToPrimAlts _ = Nothing



-- | Primitive return, but algebraic alternative on return frame
primReturnToAlgAlts :: StgState -> Maybe StgState
primReturnToAlgAlts s@StgState
    { stgCode  = ReturnInt _
    , stgStack = ReturnFrame (Alts AlgebraicAlts{} _) _ :< _ }

  = Just (s { stgInfo = Info (StateError PrimReturnToAlgAlts) [] })

primReturnToAlgAlts _ = Nothing



-- | A black hole was entered, and the infinite recursion detection triggered
-- as a result
enterBlackhole :: StgState -> Maybe StgState
enterBlackhole s@StgState
    { stgCode  = Enter addr
    , stgHeap  = heap }
    | Just (Blackhole bhTick) <- H.lookup addr heap

  = Just (s
        { stgInfo = Info (StateError EnterBlackhole)
                       [Detail_EnterBlackHole addr bhTick] })

enterBlackhole _ = Nothing



-- | Closures are always lifted, not primitive
updateClosureWithPrimitive :: StgState -> Maybe StgState
updateClosureWithPrimitive s@StgState
    { stgCode  = ReturnInt _
    , stgStack = UpdateFrame _ :< _}

  = Just (s
        { stgInfo  = Info (StateError UpdateClosureWithPrimitive)
                        [Detail_UpdateClosureWithPrimitive] })

updateClosureWithPrimitive _ = Nothing



-- | Non-algebraic scrutinee
--
-- For more information on this, see 'Stg.Prelude.seq'.
nonAlgPrimScrutinee :: StgState -> Maybe StgState
nonAlgPrimScrutinee s@StgState
    { stgCode  = Enter _
    , stgStack = ReturnFrame{} :< _}

  = Just (s { stgInfo  = Info (StateError NonAlgPrimScrutinee) [] })

nonAlgPrimScrutinee _ = Nothing



-- | A primitive division had zero as denominator
divisionByZero :: StgState -> Maybe StgState
divisionByZero s@StgState
    { stgCode = Eval (AppP op x y) locals }
    | Success (PrimInt xVal) <- localVal locals x
    , Success (PrimInt yVal) <- localVal locals y
    , Failure Div0 <- applyPrimOp op xVal yVal

  = Just (s { stgInfo  = Info (StateError DivisionByZero) [] })

divisionByZero _ = Nothing



-- | Bad constructor arity: different number of arguments in code segment and in
-- return frame
badConArity :: StgState -> Maybe StgState
badConArity s@StgState
    { stgCode  = ReturnCon con ws
    , stgStack = ReturnFrame alts _ :< _ }
    | Success (AltMatches (AlgebraicAlt _con vars _)) <- lookupAlgebraicAlt alts con
    , length ws /= length vars

  = Just (s { stgInfo  = Info (StateError (BadConArity (length ws) (length vars)))
                                          [Detail_BadConArity] })

badConArity _ = Nothing
