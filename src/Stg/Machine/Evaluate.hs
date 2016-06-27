{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Evaluate STG 'Program's.
module Stg.Machine.Evaluate (
    evalStep,
) where



import           Data.Bifunctor
import qualified Data.Foldable  as F
import qualified Data.List      as L
import qualified Data.Map       as M
import           Data.Monoid    hiding (Alt)

import           Data.Stack        (Stack (..), (<>>))
import qualified Data.Stack        as S
import           Stg.Language
import           Stg.Machine.Env
import qualified Stg.Machine.Heap  as H
import           Stg.Machine.Types
import           Stg.Util



-- | Smart constructor to avoid generating info if nothing was discarded
mkDetail_UnusedLocalVariables :: [Var] -> Locals -> [InfoDetail]
mkDetail_UnusedLocalVariables usedVars locals =
    [ Detail_UnusedLocalVariables usedVars locals
    | let Locals localsMap = locals
          used = M.fromList [ (var, ()) | var <- usedVars ]
          unused = localsMap `M.difference` used
    , not (M.null unused) && not (M.null localsMap) ]

-- | Successful alternative match, used for finding the right branch in @case@
data AltMatch alt = AltMatches alt | DefaultMatches DefaultAlt

data AltError = BadAlt -- ^ Alg/prim alternative in prim/alg case

-- | Look up an algebraic constructor among the given alternatives, and return
-- the first match. If nothing matches, return the default alternative.
lookupAlgebraicAlt
    :: Alts
    -> Constr
    -> Validate AltError (AltMatch AlgebraicAlt)
lookupAlgebraicAlt (Alts (AlgebraicAlts alts) def) constr
  = let matchingAlt (AlgebraicAlt c _ _) = c == constr
    in Success (case L.find matchingAlt alts of
        Just alt   -> AltMatches alt
        _otherwise -> DefaultMatches def )
lookupAlgebraicAlt (Alts PrimitiveAlts{} _) _ = Failure BadAlt
lookupAlgebraicAlt (Alts NoNonDefaultAlts{} def) _ = Success (DefaultMatches def)

-- | 'lookupAlgebraicAlt' for primitive literals.
lookupPrimitiveAlt
    :: Alts
    -> Literal
    -> Validate AltError (AltMatch PrimitiveAlt)
lookupPrimitiveAlt (Alts (PrimitiveAlts alts) def) lit
  = let matchingAlt (PrimitiveAlt lit' _) = lit' == lit
    in Success (case L.find matchingAlt alts of
        Just alt   -> AltMatches alt
        _otherwise -> DefaultMatches def )
lookupPrimitiveAlt (Alts AlgebraicAlts{} _) _ = Failure BadAlt
lookupPrimitiveAlt (Alts NoNonDefaultAlts{} def) _ = Success (DefaultMatches def)

liftLambdaToClosure :: Locals -> LambdaForm -> Validate NotInScope Closure
liftLambdaToClosure localsLift lf@(LambdaForm free _ _ _) =
    case traverse (first (:[]) . localVal localsLift . AtomVar) free of
        Success freeVals   -> Success (Closure lf freeVals)
        Failure notInScope -> Failure (mconcat notInScope)

data PrimError = Div0

applyPrimOp :: PrimOp -> Integer -> Integer -> Validate PrimError Integer
applyPrimOp Div _ 0 = Failure Div0
applyPrimOp Mod _ 0 = Failure Div0
applyPrimOp op x y = Success (opToFunc op x y)
  where
    boolToPrim p a b = if p a b then 1 else 0
    opToFunc = \case
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

isArgFrame :: StackFrame -> Bool
isArgFrame ArgumentFrame{} = True
isArgFrame _else           = False



-- | Perform a single STG machine evaluation step.
evalStep :: StgState -> StgState
evalStep state = let state' = stgRule state
                 in state' { stgSteps = stgSteps state' + 1 }



-- | Apply a single STG evaluation rule, as specified in the 1992 paper.
stgRule :: StgState -> StgState



-- (1) Function application
stgRule s@StgState
    { stgCode    = Eval (AppF f xs) locals
    , stgStack   = stack
    , stgGlobals = globals }
    | Success (Addr addr) <- val locals globals (AtomVar f)
    , Success xsVals <- vals locals globals xs

  = let stack' = map ArgumentFrame xsVals <>> stack

    in s { stgCode  = Enter addr
         , stgStack = stack'
         , stgInfo  = Info
             (StateTransition Eval_FunctionApplication)
             ( Detail_FunctionApplication f xs
             : mkDetail_UnusedLocalVariables (f : [ var | AtomVar var <- xs ]) locals )}



-- (2) Enter non-updatable closure
stgRule s@StgState
    { stgCode  = Enter addr
    , stgStack = stack
    , stgHeap  = heap }
    | Just (HClosure (Closure (LambdaForm free NoUpdate bound body) freeVals))
        <- H.lookup addr heap
    , Just (frames, stack') <- bound `S.forEachPop` stack
    , all isArgFrame frames
    , args <- [ arg | ArgumentFrame arg <- frames ]

  = let locals = makeLocals (freeLocals <> boundLocals)
        freeLocals = zipWith Mapping free freeVals
        boundLocals = zipWith Mapping bound args

    in s { stgCode  = Eval body locals
         , stgStack = stack'
         , stgInfo  = Info (StateTransition Enter_NonUpdatableClosure)
                           [Detail_EnterNonUpdatable addr boundLocals] }



-- (3) let(rec)
stgRule s@StgState
    { stgCode = Eval (Let rec (Binds letBinds) expr) locals
    , stgHeap = heap }

  = let (letVars, letLambdaForms) = unzip (M.assocs letBinds)

        -- We'll need the memory addresses to be created on the heap at this
        -- point already, so we pre-allocate enough already. If everything goes
        -- fine (i.e. all variables referenced in the 'let' are in scope), these
        -- dummy objects can later be overwritten by the actual closures formed
        -- in the 'let' block.
        (newAddrs, heapWithPreallocations) =
            let preallocatedObjs = map (const (Blackhole 0)) letVars
            in H.allocMany preallocatedObjs heap

        -- The local environment enriched by the definitions in the 'let'.
        locals' = let varToAddr var addr = Mapping var (Addr addr)
                      newMappings = zipWith varToAddr letVars newAddrs
                  in makeLocals newMappings <> locals

        -- The local environment applicable in the lambda forms defined in the
        -- 'let' binding.
        localsRhs = case rec of
            NonRecursive -> locals  -- New bindings are invisible
            Recursive    -> locals' -- New bindings are in scope

    in case traverse (liftLambdaToClosure localsRhs) letLambdaForms of
        Success closures ->
                -- As promised above, the preallocated dummy closures are now
                -- discarded, and replaced with the newly formed closures.
            let addrToClosure addr closure = Mapping addr (HClosure closure)
                heap' = H.updateMany
                    (zipWith addrToClosure newAddrs closures)
                    heapWithPreallocations
            in s { stgCode = Eval expr locals'
                 , stgHeap = heap'
                 , stgInfo = Info (StateTransition (Eval_Let rec))
                                  [Detail_EvalLet letVars newAddrs] }
        Failure notInScope ->
            s { stgInfo = Info (StateError (VariablesNotInScope notInScope)) [] }



-- (18, 19) Shortcut for matching primops, given before the general case rule
-- (4) so it takes precedence.
--
-- This rule allows evaluating primops without the overhead of allocating an
-- intermediate return stack frame.
--
-- When reading the source here for educational purposes, you should skip this
-- rule until you've seen the normal case rule (4) and the normal
-- primop rule (14).
--
-- This rule has the slight modification compared to the paper in that it works
-- for both bound and unbound default cases.
stgRule s@StgState
    { stgCode = Eval (Case (AppP op x y) alts) locals }
    | Success (PrimInt xVal) <- localVal locals x
    , Success (PrimInt yVal) <- localVal locals y
    , Success opXY <- applyPrimOp op xVal yVal
    , Success altLookup <- lookupPrimitiveAlt alts (Literal opXY)

  = let (locals', expr) = case altLookup of
            DefaultMatches (DefaultBound pat e)
                -> (addLocals [Mapping pat (PrimInt opXY)] locals, e)
            DefaultMatches (DefaultNotBound e)
                -> (locals, e)
            AltMatches (PrimitiveAlt _opXY e)
                -> (locals, e)

    in s { stgCode = Eval expr locals'
         , stgInfo = Info (StateTransition Eval_Case_Primop_DefaultBound) [] }



-- (4) Case evaluation
stgRule s@StgState
    { stgCode  = Eval (Case expr alts) locals
    , stgStack = stack }

  = let stack' = ReturnFrame alts locals :< stack

    in s { stgCode  = Eval expr locals
         , stgStack = stack'
         , stgInfo  = Info (StateTransition Eval_Case)
                           [Detail_EvalCase] }



-- (5) Constructor application
stgRule s@StgState
    { stgCode    = Eval (AppC con xs) locals
    , stgGlobals = globals }
    | Success valsXs <- vals locals globals xs

  = s { stgCode = ReturnCon con valsXs
      , stgInfo = Info
          (StateTransition Eval_AppC)
          (mkDetail_UnusedLocalVariables [ var | AtomVar var <- xs ] locals) }



-- (6) Algebraic constructor return, standard match
stgRule s@StgState
    { stgCode  = ReturnCon con ws
    , stgStack = ReturnFrame alts locals :< stack' }
    | Success (AltMatches (AlgebraicAlt _con vars expr)) <-
        lookupAlgebraicAlt alts con
    , length ws == length vars

  = let locals' = addLocals (zipWith Mapping vars ws) locals

    in s { stgCode  = Eval expr locals'
         , stgStack = stack'
         , stgInfo  = Info (StateTransition ReturnCon_Match)
                           [Detail_ReturnCon_Match con vars] }




-- (7) Algebraic constructor return, unbound default match
stgRule s@StgState
    { stgCode  = ReturnCon con _ws
    , stgStack = ReturnFrame alts locals :< stack' }
    | Success (DefaultMatches (DefaultNotBound expr)) <-
        lookupAlgebraicAlt alts con

  = s { stgCode  = Eval expr locals
      , stgStack = stack'
      , stgInfo  = Info (StateTransition ReturnCon_DefUnbound) [] }



-- (8) Algebraic constructor return, bound default match
stgRule s@StgState
    { stgCode  = ReturnCon con ws
    , stgStack = ReturnFrame alts locals :< stack'
    , stgHeap  = heap
    , stgSteps = steps }
    | Success (DefaultMatches (DefaultBound v expr)) <-
        lookupAlgebraicAlt alts con

  = let locals' = addLocals [Mapping v (Addr addr)] locals
        (addr, heap') = H.alloc (HClosure closure) heap
        closure = Closure (LambdaForm vs NoUpdate [] (AppC con (map AtomVar vs))) ws
        vs = let newVar _old i = Var ("alg8_" <> show' steps <> "-" <> show' i)
             in zipWith newVar ws [0::Integer ..]
    in s { stgCode  = Eval expr locals'
         , stgStack = stack'
         , stgHeap  = heap'
         , stgInfo  = Info (StateTransition ReturnCon_DefBound)
                           [Detail_ReturnConDefBound v addr] }



-- (9) Literal evaluation
stgRule s@StgState { stgCode = Eval (Lit (Literal k)) _locals}
  = s { stgCode = ReturnInt k
      , stgInfo = Info (StateTransition Eval_Lit) [] }



-- (10) Literal application
stgRule s@StgState { stgCode = Eval (AppF f []) locals }
    | Success (PrimInt k) <- val locals mempty (AtomVar f)

  = s { stgCode = ReturnInt k
      , stgInfo = Info (StateTransition Eval_LitApp)
                       (mkDetail_UnusedLocalVariables [f] locals) }



-- (11) Primitive return, standard match found
stgRule s@StgState
    { stgCode  = ReturnInt k
    , stgStack = ReturnFrame alts locals :< stack' }
    | Success (AltMatches (PrimitiveAlt _k expr)) <-
        lookupPrimitiveAlt alts (Literal k)

  = s { stgCode  = Eval expr locals
      , stgStack = stack'
      , stgInfo  = Info (StateTransition ReturnInt_Match) [] }



-- (12) Primitive return, bound default match
stgRule s@StgState
    { stgCode  = ReturnInt k
    , stgStack = ReturnFrame alts locals :< stack' }
    | Success (DefaultMatches (DefaultBound v expr)) <-
        lookupPrimitiveAlt alts (Literal k)

  = let locals' = addLocals [Mapping v (PrimInt k)] locals

    in s { stgCode  = Eval expr locals'
         , stgStack = stack'
         , stgInfo  = Info (StateTransition ReturnInt_DefBound)
                           [Detail_ReturnIntDefBound v k] }



-- (13) Primitive return, unbound default match
stgRule s@StgState
    { stgCode  = ReturnInt k
    , stgStack = ReturnFrame alts locals :< stack' }
    | Success (DefaultMatches (DefaultNotBound expr)) <-
        lookupPrimitiveAlt alts (Literal k)

  = s { stgCode  = Eval expr locals
      , stgStack = stack'
      , stgInfo  = Info (StateTransition ReturnInt_DefUnbound) [] }



-- (14) Primitive function application
--
-- This rule has been modified to take not only primitive-valued variables, but
-- also primitive values directly as arguments.
--
-- Without this modification, you cannot evaluate @+# 1# 2#@, you have to
-- write
--
-- @
-- case 1# of one -> case 2# of two -> case +# one two of ...
-- @
--
-- which is a bit silly. I think this might be an oversight in the 1992 paper.
-- The fast curry paper does not seem to impose this restriction.
--
--
-- TODO: This rule is probably obsolete because of rules (18) and (19).
-- Remove it after confirming this is true. I (quchen) was not able to produce
-- a case in which (14) is still needed.
stgRule s@StgState
    { stgCode = Eval (AppP op x y) locals }
    | Success (PrimInt xVal) <- localVal locals x
    , Success (PrimInt yVal) <- localVal locals y
    , Success result <- applyPrimOp op xVal yVal

  = s { stgCode = ReturnInt result
      , stgInfo = Info (StateTransition Eval_AppP)
                       (mkDetail_UnusedLocalVariables [var | AtomVar var <- [x,y]]
                                                      locals )}



-- (15) Enter updatable closure
stgRule s@StgState
    { stgCode  = Enter addr
    , stgStack = stack
    , stgHeap  = heap
    , stgSteps = tick }
    | Just (HClosure (Closure (LambdaForm free Update [] body) freeVals))
        <- H.lookup addr heap

  = let stack' = UpdateFrame addr :< stack
        locals = makeLocals (zipWith Mapping free freeVals)
        heap' = H.update (Mapping addr (Blackhole tick)) heap

    in s { stgCode  = Eval body locals
         , stgStack = stack'
         , stgHeap  = heap'
         , stgInfo  = Info (StateTransition Enter_UpdatableClosure)
                           [Detail_EnterUpdatable addr] }



-- (16) Algebraic constructor return, argument/return stacks empty -> update
stgRule s@StgState
    { stgCode  = ReturnCon con ws
    , stgStack = UpdateFrame addr :< stack'
    , stgHeap  = heap
    , stgSteps = steps }

  = let vs = let newVar _old i = Var ("upd16_" <> show' steps <> "-" <> show' i)
             in zipWith newVar ws [0::Integer ..]
        lf = LambdaForm vs NoUpdate [] (AppC con (map AtomVar vs))
        heap' = H.update (Mapping addr (HClosure (Closure lf ws))) heap

    in s { stgCode  = ReturnCon con ws
         , stgStack = stack'
         , stgHeap  = heap'
         , stgInfo  = Info (StateTransition ReturnCon_Update)
                           [Detail_ConUpdate con addr] }



-- (17a) Enter partially applied closure
stgRule s@StgState
    { stgCode  = Enter addrEnter
    , stgStack = stack
    , stgHeap  = heap
    , stgSteps = steps }
    | Just (HClosure (Closure (LambdaForm _vs NoUpdate xs _body) _wsf))
        <- H.lookup addrEnter heap
    , Just (argFrames, UpdateFrame addrUpdate :< stack')
        <- popArgsUntilUpdate stack

  = let xs1 = zipWith const xs (F.toList argFrames)
        f = Var ("upd17a_" <> show' steps)
        fxs1 = AppF f (map AtomVar xs1)
        freeVars = f : xs1
        freeVals = zipWith const
            (Addr addrEnter : F.foldMap (\(ArgumentFrame v) -> [v]) argFrames)
            freeVars
        updatedClosure = Closure (LambdaForm freeVars NoUpdate [] fxs1) freeVals

        heap' = H.update (Mapping addrUpdate (HClosure updatedClosure)) heap

    in s { stgCode  = Enter addrEnter
         , stgStack = argFrames <>> stack'
         , stgHeap  = heap'
         , stgInfo  = Info (StateTransition Enter_PartiallyAppliedUpdate)
                           [Detail_PapUpdate addrUpdate] }

  where

    -- | Are there enough 'ArgumentFrame's on the stack to fill the args
    -- parameter? If so, return those frames, along with the rest of the stack.
    popArgsUntilUpdate withArgsStack
        = let (argFrames, argsPoppedStack) = S.span isArgFrame withArgsStack
          in Just ( filter isArgFrame (F.toList argFrames)
                  , argsPoppedStack )



stgRule s = noRuleApplies s



-- | When there are no rules, the machine halts. But there are many different
-- ways this state can be reached, so it's helpful to the user to distinguish
-- them from each other.
noRuleApplies :: StgState -> StgState

-- Page 39, 2nd paragraph: "[...] closures with non-empty argument lists are
-- never updatable [...]"
noRuleApplies s@StgState
    { stgCode = Enter addr
    , stgHeap = heap }
    | Just (HClosure (Closure (LambdaForm _ Update (_:_) _) _))
        <- H.lookup addr heap
  = s { stgInfo = Info (StateError UpdatableClosureWithArgs) [] }



-- Page 39, 4th paragraph: "It is not possible for the ReturnInt state to see an
-- empty return stack, because that would imply that a closure should be updated
-- with a primitive value; but no closure has a primitive type."
noRuleApplies s@StgState
    { stgCode  = ReturnInt{}
    , stgStack = Empty }

  = s { stgInfo = Info (StateError ReturnIntWithEmptyReturnStack)
                       [Detail_ReturnIntCannotUpdate] }



-- Function argument not in scope
noRuleApplies s@StgState
    { stgCode    = Eval (AppF f xs) locals
    , stgGlobals = globals }
    | Failure notInScope <- vals locals globals (AtomVar f : xs)

  = s { stgInfo = Info (StateError (VariablesNotInScope notInScope)) [] }



-- Constructor argument not in scope
noRuleApplies s@StgState
    { stgCode    = Eval (AppC _con xs) locals
    , stgGlobals = globals }
    | Failure notInScope <- vals locals globals xs

  = s { stgInfo = Info (StateError (VariablesNotInScope notInScope)) [] }



-- Algebraic constructor return, but primitive alternative on return frame
noRuleApplies s@StgState
    { stgCode  = ReturnCon{}
    , stgStack = ReturnFrame (Alts PrimitiveAlts{} _) _ :< _ }

  = s { stgInfo = Info (StateError AlgReturnToPrimAlts) [] }



-- Primitive return, but algebraic alternative on return frame
noRuleApplies s@StgState
    { stgCode  = ReturnInt _
    , stgStack = ReturnFrame (Alts AlgebraicAlts{} _) _ :< _ }

  = s { stgInfo = Info (StateError PrimReturnToAlgAlts) [] }



noRuleApplies s@StgState
    { stgCode = Eval (AppP _op x y) locals }
    | Failure notInScope <- traverse (localVal locals) ([x,y] :: [Atom])

  = s { stgInfo = Info (StateError (VariablesNotInScope notInScope)) [] }



-- Entering a black hole
noRuleApplies s@StgState
    { stgCode  = Enter addr
    , stgHeap  = heap }
    | Just (Blackhole bhTick) <- H.lookup addr heap

  = s { stgInfo = Info (StateError EnterBlackhole)
                       [Detail_EnterBlackHole addr bhTick] }



-- Update closure with primitive value
noRuleApplies s@StgState
    { stgCode  = ReturnInt _
    , stgStack = UpdateFrame _ :< _}

  = s { stgInfo  = Info (StateError UpdateClosureWithPrimitive)
                        [Detail_UpdateClosureWithPrimitive] }



-- Non-algebraic scrutinee
--
-- For more information on this, see 'Stg.Prelude.seq'.
noRuleApplies s@StgState
    { stgCode  = Enter _
    , stgStack = ReturnFrame{} :< _}

  = s { stgInfo  = Info (StateError NonAlgPrimScrutinee) [] }


noRuleApplies s@StgState
    { stgCode = Eval (AppP op x y) locals }
    | Success (PrimInt xVal) <- localVal locals x
    , Success (PrimInt yVal) <- localVal locals y
    , Failure Div0 <- applyPrimOp op xVal yVal

  = s { stgInfo  = Info (StateError DivisionByZero) [] }

-- Bad constructor arity: different number of arguments in code segment
-- and in return frame
noRuleApplies s@StgState
    { stgCode  = ReturnCon con ws
    , stgStack = ReturnFrame alts _ :< _ }
    | Success (AltMatches (AlgebraicAlt _con vars _)) <- lookupAlgebraicAlt alts con
    , length ws /= length vars

  = s { stgInfo  = Info (StateError (BadConArity (length ws) (length vars)))
                                    [Detail_BadConArity] }





-- Successful, ordinary termination
noRuleApplies s@StgState { stgStack = S.Empty }
  = s { stgInfo = Info NoRulesApply [] }



noRuleApplies s = s { stgInfo = Info NoRulesApply [Detail_StackNotEmpty] }
