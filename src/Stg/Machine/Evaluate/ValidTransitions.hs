{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Valid transitions from one state to another. Error conditions are handled
-- separately in "ErrorTransitions" if possible.
module Stg.Machine.Evaluate.ValidTransitions (
    rule1,
    rule2,
    rule3,
    rule4,
    rule5,
    rule6,
    rule7,
    rule8,
    rule9,
    rule10,
    rule11,
    rule12,
    rule13,
    rule14,
    rule15,
    rule16,
    rule17a,
    rule1819,
) where



import           Data.Bifunctor
import qualified Data.Foldable  as F
import qualified Data.Map       as M
import           Data.Monoid    hiding (Alt)
import qualified Data.Set       as S

import           Data.Stack                  (Stack (..), (<>>))
import qualified Data.Stack                  as S
import           Stg.Language
import           Stg.Machine.Env
import           Stg.Machine.Evaluate.Common
import qualified Stg.Machine.Heap            as H
import           Stg.Machine.Types
import           Stg.StaticAnalysis
import           Stg.Util



-- | Smart constructor to avoid generating info if nothing was discarded
mkDetail_UnusedLocalVariables :: [Var] -> Locals -> [InfoDetail]
mkDetail_UnusedLocalVariables usedVars locals =
    [ Detail_UnusedLocalVariables usedVars locals
    | let Locals localsMap = locals
          used = M.fromList [ (var, ()) | var <- usedVars ]
          unused = localsMap `M.difference` used
    , not (M.null unused) && not (M.null localsMap) ]



-- | (1) Function application
rule1 :: StgState -> Maybe StgState
rule1 s@StgState
    { stgCode    = Eval (AppF f xs) locals
    , stgStack   = stack
    , stgGlobals = globals }
    | Success (Addr addr) <- val locals globals (AtomVar f)
    , Success xsVals <- vals locals globals xs

  = let stack' = map ArgumentFrame xsVals <>> stack

    in Just (s
        { stgCode  = Enter addr
        , stgStack = stack'
        , stgInfo  = Info
            (StateTransition Eval_FunctionApplication)
            ( Detail_FunctionApplication f xs
            : mkDetail_UnusedLocalVariables (f : [ var | AtomVar var <- xs ]) locals )})

rule1 _ = Nothing



isArgFrame :: StackFrame -> Bool
isArgFrame ArgumentFrame{} = True
isArgFrame _else           = False



-- | (2) Enter non-updatable closure
rule2 :: StgState -> Maybe StgState
rule2 s@StgState
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

    in Just (s
        { stgCode  = Eval body locals
        , stgStack = stack'
        , stgInfo  = Info (StateTransition Enter_NonUpdatableClosure)
                          [Detail_EnterNonUpdatable addr boundLocals] })

rule2 _ = Nothing



-- | Create a 'Closure' out of a 'LambdaForm', given a local environment.
liftLambdaToClosure :: Locals -> LambdaForm -> Validate NotInScope Closure
liftLambdaToClosure locals lf@(LambdaForm free _ _ _) =
    case traverse (first (:[]) . localVal locals . AtomVar) free of
        Success freeVals   -> Success (Closure lf freeVals)
        Failure notInScope -> Failure (mconcat notInScope)



-- | (3) let(rec)
rule3 :: StgState -> Maybe StgState
rule3 s@StgState
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

    in Just (case traverse (liftLambdaToClosure localsRhs) letLambdaForms of
        Success closures ->
                -- As promised above, the preallocated dummy closures are
                -- now discarded, and replaced with the newly formed
                -- closures.
            let addrToClosure addr closure = Mapping addr (HClosure closure)
                heap' = H.updateMany
                    (zipWith addrToClosure newAddrs closures)
                    heapWithPreallocations
            in s { stgCode = Eval expr locals'
                 , stgHeap = heap'
                 , stgInfo = Info (StateTransition (Eval_Let rec))
                                  [Detail_EvalLet letVars newAddrs] }
        Failure notInScope ->
            s { stgInfo = Info (StateError (VariablesNotInScope notInScope)) [] })

rule3 _ = Nothing



-- | (4) Case evaluation
--
-- Compared to the paper, this rule was improved by removing local bindings that
-- are not used at all in the alternatives, which would unnecessarily prolong
-- the garbage collection lifetime of unused bindings.
rule4 :: StgState -> Maybe StgState
rule4 s@StgState
    { stgCode  = Eval (Case expr alts) locals
    , stgStack = stack }

  = let stack' = ReturnFrame alts (removeUnusedLocals locals) :< stack

        removeUnusedLocals (Locals unpackedLocals) =
            let freeInBody var _val = var `S.member` freeVariables alts
            in Locals (M.filterWithKey freeInBody unpackedLocals)

    in Just (s
        { stgCode  = Eval expr locals
        , stgStack = stack'
        , stgInfo  = Info (StateTransition Eval_Case)
                          [Detail_EvalCase] })

rule4 _ = Nothing



-- | (5) Constructor application
rule5 :: StgState -> Maybe StgState
rule5 s@StgState
    { stgCode    = Eval (AppC con xs) locals
    , stgGlobals = globals }
    | Success valsXs <- vals locals globals xs

  = Just (s
        { stgCode = ReturnCon con valsXs
        , stgInfo = Info
            (StateTransition Eval_AppC)
            (mkDetail_UnusedLocalVariables [ var | AtomVar var <- xs ] locals) })

rule5 _ = Nothing



-- | (6) Algebraic constructor return, standard matchrule
rule6 :: StgState -> Maybe StgState
rule6 s@StgState
    { stgCode  = ReturnCon con ws
    , stgStack = ReturnFrame alts locals :< stack' }
    | Success (AltMatches (AlgebraicAlt _con vars expr)) <-
        lookupAlgebraicAlt alts con
    , length ws == length vars

  = let locals' = addLocals (zipWith Mapping vars ws) locals

    in Just (s
        { stgCode  = Eval expr locals'
        , stgStack = stack'
        , stgInfo  = Info (StateTransition ReturnCon_Match)
                          [Detail_ReturnCon_Match con vars] })

rule6 _ = Nothing



-- | (7) Algebraic constructor return, unbound default matchrule
rule7 :: StgState -> Maybe StgState
rule7 s@StgState
    { stgCode  = ReturnCon con _ws
    , stgStack = ReturnFrame alts locals :< stack' }
    | Success (DefaultMatches (DefaultNotBound expr)) <-
        lookupAlgebraicAlt alts con

  = Just (s
        { stgCode  = Eval expr locals
        , stgStack = stack'
        , stgInfo  = Info (StateTransition ReturnCon_DefUnbound) [] })

rule7 _ = Nothing



-- | (8) Algebraic constructor return, bound default matchrule
rule8 :: StgState -> Maybe StgState
rule8 s@StgState
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
    in Just (s { stgCode  = Eval expr locals'
         , stgStack = stack'
         , stgHeap  = heap'
         , stgInfo  = Info (StateTransition ReturnCon_DefBound)
                           [Detail_ReturnConDefBound v addr] })

rule8 _ = Nothing



-- | (9) Literal evaluationrule
rule9 :: StgState -> Maybe StgState
rule9 s@StgState { stgCode = Eval (Lit (Literal k)) _locals}
  = Just (s
        { stgCode = ReturnInt k
        , stgInfo = Info (StateTransition Eval_Lit) [] })

rule9 _ = Nothing



-- | (10) Literal application
rule10 :: StgState -> Maybe StgState
rule10 s@StgState { stgCode = Eval (AppF f []) locals }
    | Success (PrimInt k) <- val locals mempty (AtomVar f)

  = Just (s
        { stgCode = ReturnInt k
        , stgInfo = Info (StateTransition Eval_LitApp)
                         (mkDetail_UnusedLocalVariables [f] locals) })

rule10 _ = Nothing



-- | (11) Primitive return, standard match found
rule11 :: StgState -> Maybe StgState
rule11 s@StgState
    { stgCode  = ReturnInt k
    , stgStack = ReturnFrame alts locals :< stack' }
    | Success (AltMatches (PrimitiveAlt _k expr)) <-
        lookupPrimitiveAlt alts (Literal k)

  = Just (s
        { stgCode  = Eval expr locals
        , stgStack = stack'
        , stgInfo  = Info (StateTransition ReturnInt_Match) [] })

rule11 _ = Nothing



-- | (12) Primitive return, bound default match
rule12 :: StgState -> Maybe StgState
rule12 s@StgState
    { stgCode  = ReturnInt k
    , stgStack = ReturnFrame alts locals :< stack' }
    | Success (DefaultMatches (DefaultBound v expr)) <-
        lookupPrimitiveAlt alts (Literal k)

  = let locals' = addLocals [Mapping v (PrimInt k)] locals

    in Just ( s
        { stgCode  = Eval expr locals'
        , stgStack = stack'
        , stgInfo  = Info (StateTransition ReturnInt_DefBound)
                          [Detail_ReturnIntDefBound v k] })

rule12 _ = Nothing



-- | (13) Primitive return, unbound default match
rule13 :: StgState -> Maybe StgState
rule13 s@StgState
    { stgCode  = ReturnInt k
    , stgStack = ReturnFrame alts locals :< stack' }
    | Success (DefaultMatches (DefaultNotBound expr)) <-
        lookupPrimitiveAlt alts (Literal k)

  = Just (s
        { stgCode  = Eval expr locals
        , stgStack = stack'
        , stgInfo  = Info (StateTransition ReturnInt_DefUnbound) [] })

rule13 _ = Nothing



-- | (14) Primitive function application
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
rule14 :: StgState -> Maybe StgState
rule14 s@StgState
    { stgCode = Eval (AppP op x y) locals }
    | Success (PrimInt xVal) <- localVal locals x
    , Success (PrimInt yVal) <- localVal locals y
    , Success result <- applyPrimOp op xVal yVal

  = Just (s
        { stgCode = ReturnInt result
        , stgInfo = Info (StateTransition Eval_AppP)
                         (mkDetail_UnusedLocalVariables [var | AtomVar var <- [x,y]]
                                                        locals )})

rule14 _ = Nothing



-- | (15) Enter updatable closure
rule15 :: StgState -> Maybe StgState
rule15 s@StgState
    { stgCode  = Enter addr
    , stgStack = stack
    , stgHeap  = heap
    , stgSteps = tick }
    | Just (HClosure (Closure (LambdaForm free Update [] body) freeVals))
        <- H.lookup addr heap

  = let stack' = UpdateFrame addr :< stack
        locals = makeLocals (zipWith Mapping free freeVals)
        heap' = H.update (Mapping addr (Blackhole tick)) heap

    in Just ( s
        { stgCode  = Eval body locals
        , stgStack = stack'
        , stgHeap  = heap'
        , stgInfo  = Info (StateTransition Enter_UpdatableClosure)
                          [Detail_EnterUpdatable addr] })

rule15 _ = Nothing



-- | (16) Algebraic constructor return, argument/return stacks empty -> update
rule16 :: StgState -> Maybe StgState
rule16 s@StgState
    { stgCode  = ReturnCon con ws
    , stgStack = UpdateFrame addr :< stack'
    , stgHeap  = heap
    , stgSteps = steps }

  = let vs = let newVar _old i = Var ("upd16_" <> show' steps <> "-" <> show' i)
             in zipWith newVar ws [0::Integer ..]
        lf = LambdaForm vs NoUpdate [] (AppC con (map AtomVar vs))
        heap' = H.update (Mapping addr (HClosure (Closure lf ws))) heap

    in Just (s
        { stgCode  = ReturnCon con ws
        , stgStack = stack'
        , stgHeap  = heap'
        , stgInfo  = Info (StateTransition ReturnCon_Update)
                          [Detail_ConUpdate con addr] })

rule16 _ = Nothing



-- | (17a) Enter partially applied closure
rule17a :: StgState -> Maybe StgState
rule17a s@StgState
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

    in Just (s
        { stgCode  = Enter addrEnter
        , stgStack = argFrames <>> stack'
        , stgHeap  = heap'
        , stgInfo  = Info (StateTransition Enter_PartiallyAppliedUpdate)
                          [Detail_PapUpdate addrUpdate] })

  where

    -- | Are there enough 'ArgumentFrame's on the stack to fill the args
    -- parameter? If so, return those frames, along with the rest of the stack.
    popArgsUntilUpdate withArgsStack
        = let (argFrames, argsPoppedStack) = S.span isArgFrame withArgsStack
          in Just ( filter isArgFrame (F.toList argFrames)
                  , argsPoppedStack )

rule17a _ = Nothing



-- | (18, 19) Shortcut for matching primops, given before the general case rule
-- (4) so it takes precedence.
--
-- This rule allows evaluating primops without the overhead of allocating an
-- intermediate return stack frame. In order to trigger, it must be placed
-- before rule 4 (general case evaluation) in order to take precedence.
--
-- When reading the source here for educational purposes, you should skip this
-- rule until you've seen the normal case rule (4) and the normal
-- primop rule (14).
--
-- This rule has the slight modification compared to the paper in that it works
-- for both bound and unbound default cases.
rule1819 :: StgState -> Maybe StgState
rule1819 s@StgState
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

    in Just (
        s { stgCode = Eval expr locals'
          , stgInfo = Info (StateTransition Eval_Case_Primop_DefaultBound) [] })

rule1819 _ = Nothing
