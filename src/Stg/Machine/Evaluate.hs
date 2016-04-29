{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Evaluate STG 'Program's.
module Stg.Machine.Evaluate (
    evalStep,
) where



import           Data.Bifunctor
import qualified Data.Foldable            as F
import qualified Data.List                as L
import qualified Data.Map                 as M
import           Data.Monoid              hiding (Alt)
import qualified Data.Text                as T

import           Stack                    (Stack (..), (<>>))
import qualified Stack                    as S
import           Stg.Language
import           Stg.Language.Prettyprint
import           Stg.Machine.Env
import qualified Stg.Machine.Heap         as H
import           Stg.Machine.Types
import           Stg.Parser
import           Stg.Util



-- | Look up an algebraic constructor among the given alternatives, and return
-- the first match. If nothing matches, return the default alternative.
lookupAlgebraicAlt
    :: Alts
    -> Constr
    -> Either DefaultAlt Alt
lookupAlgebraicAlt (Alts alts def) constr  = lookupAlt matchingAlt alts def
  where
    matchingAlt (AlgebraicAlt c _ _) = c == constr
    matchingAlt (PrimitiveAlt{}) = False

-- | 'lookupPrimitiveAlt' for primitive literals.
lookupPrimitiveAlt
    :: Alts
    -> Literal
    -> Either DefaultAlt Alt
lookupPrimitiveAlt (Alts alts def) lit = lookupAlt matchingAlt alts def
  where
    matchingAlt (PrimitiveAlt lit' _) = lit' == lit
    matchingAlt (AlgebraicAlt{}) = False

lookupAlt :: (alt -> Bool) -> [alt] -> def -> Either def alt
lookupAlt matchingAlt alts def = case L.find matchingAlt alts of
    Just alt -> Right alt
    _otherwise -> Left def

liftLambdaToClosure :: Locals -> LambdaForm -> Validate NotInScope Closure
liftLambdaToClosure localsLift lf@(LambdaForm free _ _ _) =
    case traverse (first (:[]) . localVal localsLift) free of
        Success freeVals    -> Success (Closure lf freeVals)
        Failure notInScope -> Failure (mconcat notInScope)

-- | Perform a single STG machine evaluation step.
evalStep :: StgState -> StgState
evalStep state = let state' = stgRule state
                 in state' { stgTicks = stgTicks state' + 1 }



-- | Apply a single STG evaluation rule, as specified in the 1992 paper.
stgRule :: StgState -> StgState



-- (1) Function application
stgRule s@StgState
    { stgCode     = Eval (AppF f xs) locals
    , stgArgStack = argS
    , stgGlobals  = globals }
    | Success (Addr a) <- val locals globals (AtomVar f)
    , Success xsVals <- vals locals globals xs

  = let argS' = map ArgumentFrame xsVals <>> argS

    in s { stgCode     = Enter a
         , stgArgStack = argS'
         , stgInfo = Info (StateTransiton Eval_FunctionApplication)
            ((InfoDetail . mconcat)
                [ [ T.unwords
                    [ "Apply function"
                    , prettyprint f
                    , case xs of
                        [] -> "without arguments"
                        _  -> T.unwords
                            [ " to arguments "
                            , T.intercalate ", " (foldMap (\arg -> [prettyprint arg]) xs) ]]]
                , let Locals loc = locals
                      used = M.fromList [ (var, ()) | AtomVar var <- xs ]
                      discarded = loc `M.difference` used
                  in if M.null discarded
                      then []
                      else ["Unused local variables discarded: " <> T.intercalate ", " (foldMap (\var -> [prettyprint var]) discarded) ]])}
                            -- TODO: Show variable names here, not addresses



-- (2) Enter non-updatable closure
stgRule s@StgState
    { stgCode     = Enter a
    , stgArgStack = argS
    , stgHeap     = heap }
    | Just (Closure (LambdaForm free NoUpdate bound body) freeVals) <- H.lookup a heap
    , Just (args, argS') <- S.popN (length bound) argS

  = let locals = makeLocals (freeLocals <> boundLocals)
        freeLocals = zip free freeVals
        boundLocals = zipWith (\b (ArgumentFrame v) -> (b, v)) bound args

    in s { stgCode     = Eval body locals
         , stgArgStack = argS'
         , stgInfo     = Info (StateTransiton Enter_NonUpdatableClosure)
            ["Enter closure at " <> prettyprint a] }



-- (3) let(rec)
stgRule s@StgState
    { stgCode = Eval (Let rec (Binds binds) expr) locals
    , stgHeap = heap }

    -- TODO: Refactor this fugly mess, apologies for writing it - David/quchen
  = let (vars, lambdaForms) = unzip (M.assocs binds)
        dummyClosures = takeMatchingLength
            (repeat (Closure [stg| () \n () -> Let_rule_dummy () |] []))
            vars
        stuffNeeded =
            let (addrsX, heapWithDummies) = H.allocMany dummyClosures heap
                localsX' = makeLocals' vars addrsX
                v'closures = makeClosures lambdaForms localsX'
            in case v'closures of
                Success closures ->
                    let heapX' = H.updateMany addrsX closures heapWithDummies
                    in Success (localsX', addrsX, heapX')
                Failure err -> Failure err
    in case stuffNeeded of
        Success (locals', addrs, heap') ->
            s { stgCode = Eval expr locals'
              , stgHeap = heap'
              , stgInfo = Info (StateTransiton (Eval_Let rec))
                 [ T.unwords
                     [ "Local environment extended by"
                     , T.intercalate ", " (foldMap (\var -> [prettyprint var]) vars)]
                 , T.unwords
                     [ "Allocate new closures at"
                     , T.intercalate ", " (foldMap (\addr -> [prettyprint addr]) addrs)
                     , "on the heap" ]] }
        Failure (NotInScope notInScope) ->
            s { stgInfo = Info (StateError (VariablesNotInScope notInScope)) [] }
  where
    makeLocals' :: [Var] -> [MemAddr] -> Locals
    makeLocals' vars addrs = locals'
      where
        locals' = locals <> newLocals
        newLocals = makeLocals (zipWith makeLocal vars addrs)
        makeLocal n a = (n, Addr a)

    makeClosures :: [LambdaForm] -> Locals -> Validate NotInScope [Closure]
    makeClosures lambdaForms locals' = traverse (liftLambdaToClosure localsRhs) lambdaForms
      where
        localsRhs = case rec of
            NonRecursive -> locals
            Recursive    -> locals'



-- (4) Case evaluation
stgRule s@StgState
    { stgCode        = (Eval (Case expr alts) locals)
    , stgReturnStack = retS }

  = let retS' = ReturnFrame alts locals :< retS

    in s { stgCode        = Eval expr locals
         , stgReturnStack = retS'
         , stgInfo        = Info (StateTransiton Eval_Case)
            [ "Push the alternatives and the local environment on the update stack" ] }



-- (5) Constructor application
stgRule s@StgState
    { stgCode    = Eval (AppC con xs) locals
    , stgGlobals = globals }
    | Success valsXs <- vals locals globals xs

  = s { stgCode = ReturnCon con valsXs
      , stgInfo = Info (StateTransiton Eval_AppC) [] }



-- (6) Algebraic constructor return, standard match found
stgRule s@StgState
    { stgCode        = ReturnCon con ws
    , stgReturnStack = ReturnFrame alts locals :< retS' }
    | Right (AlgebraicAlt _con vars expr) <- lookupAlgebraicAlt alts con

  = let locals' = addLocals (zip vars ws) locals

    in s { stgCode        = Eval expr locals'
         , stgReturnStack = retS'
         , stgInfo        = Info (StateTransiton ReturnCon_Match) [] }



-- (7) Algebraic constructor return, unbound default match
stgRule s@StgState
    { stgCode        = ReturnCon con _ws
    , stgReturnStack = ReturnFrame alts locals :< retS' }
    | Left (DefaultNotBound expr) <- lookupAlgebraicAlt alts con

  = s { stgCode        = Eval expr locals
      , stgReturnStack = retS'
      , stgInfo        = Info (StateTransiton ReturnCon_DefUnbound) [] }



-- (8) Algebraic constructor return, bound default match
stgRule s@StgState
    { stgCode        = ReturnCon con ws
    , stgReturnStack = ReturnFrame alts locals :< retS'
    , stgHeap        = heap
    , stgTicks       = ticks }
    | Left (DefaultBound v expr) <- lookupAlgebraicAlt alts con

  = let locals' = addLocals [(v, Addr addr)] locals
        (addr, heap') = H.alloc closure heap
        closure = Closure (LambdaForm vs NoUpdate [] (AppC con (map AtomVar vs))) ws
        vs = let newVar _old i = Var ("alg8_" <> show' ticks <> "-" <> show' i)
             in zipWith newVar ws [0::Integer ..]
    in s { stgCode        = Eval expr locals'
         , stgReturnStack = retS'
         , stgHeap        = heap'
         , stgInfo        = Info (StateTransiton ReturnCon_DefBound) [] }



-- (9) Literal evaluation
stgRule s@StgState { stgCode = Eval (Lit (Literal k)) _locals}
  = s { stgCode = ReturnInt k
      , stgInfo = Info (StateTransiton Eval_Lit) [] }



-- (10) Literal application
stgRule s@StgState { stgCode = Eval (AppF f []) locals }
    | Success (PrimInt k) <- val locals mempty (AtomVar f)

  = s { stgCode = ReturnInt k
      , stgInfo = Info (StateTransiton Eval_LitApp) [] }



-- (11) Primitive constructor return, standard match found
stgRule s@StgState
    { stgCode        = ReturnInt k
    , stgReturnStack = ReturnFrame alts locals :< retS' }
    | Right (PrimitiveAlt _k expr) <- lookupPrimitiveAlt alts (Literal k)

  = s { stgCode        = Eval expr locals
      , stgReturnStack = retS'
      , stgInfo        = Info (StateTransiton ReturnInt_Match) [] }



-- (12) Primitive constructor return, bound default match
stgRule s@StgState
    { stgCode        = ReturnInt k
    , stgReturnStack = ReturnFrame alts locals :< retS' }
    | Left (DefaultBound v expr) <- lookupPrimitiveAlt alts (Literal k)

  = let locals' = addLocals [(v, PrimInt k)] locals

    in s { stgCode        = Eval expr locals'
         , stgReturnStack = retS'
         , stgInfo        = Info (StateTransiton ReturnInt_DefBound) [] }



-- (13) Primitive constructor return, unbound default match
stgRule s@StgState
    { stgCode        = ReturnInt k
    , stgReturnStack = ReturnFrame alts locals :< retS' }
    | Left (DefaultNotBound expr) <- lookupPrimitiveAlt alts (Literal k)

  = s { stgCode        = Eval expr locals
      , stgReturnStack = retS'
      , stgInfo        = Info (StateTransiton ReturnInt_DefUnbound) [] }



-- (14) Primitive function application
stgRule s@StgState
    { stgCode = Eval (AppP op (AtomVar x) (AtomVar y)) locals }
    | Success (PrimInt xVal) <- localVal locals x
    , Success (PrimInt yVal) <- localVal locals y

  = let boolToPrim p a b = if p a b then 1 else 0
        apply = \case
            Add -> (+)
            Sub -> (-)
            Mul -> (*)
            Div -> quot
            Mod -> rem
            Eq  -> boolToPrim (==)
            Lt  -> boolToPrim (<)
            Leq -> boolToPrim (<=)
            Gt  -> boolToPrim (>)
            Geq -> boolToPrim (>=)
            Neq -> boolToPrim (/=)

    in s { stgCode = ReturnInt (apply op xVal yVal)
         , stgInfo = Info (StateTransiton Eval_AppP) [] }



-- (15) Enter updatable closure
stgRule s@StgState
    { stgCode        = Enter addr
    , stgArgStack    = argS
    , stgReturnStack = retS
    , stgUpdateStack = updS
    , stgHeap        = heap }
    | Just (Closure (LambdaForm free Update [] body) freeVals) <- H.lookup addr heap

  = let updS' = UpdateFrame argS retS addr :< updS
        locals = makeLocals (zip free freeVals)

    in s { stgCode        = Eval body locals
         , stgArgStack    = Empty
         , stgReturnStack = Empty
         , stgUpdateStack = updS'
         , stgInfo        = Info (StateTransiton Enter_UpdatableClosure)
            [ "Push a new update frame with the entered address " <> prettyprint addr
            , "Save current argument and return stacks on that update frame"
            , "Argument and return stacks are now empty"  ] }



-- (16) Algebraic constructor return, argument/return stacks empty -> update
stgRule s@StgState
    { stgCode        = ReturnCon con ws
    , stgArgStack    = Empty
    , stgReturnStack = Empty
    , stgUpdateStack = UpdateFrame argSU retSU addrU :< updS'
    , stgHeap        = heap
    , stgTicks       = ticks }

  = let vs = let newVar _old i = Var ("upd16_" <> show' ticks <> "-" <> show' i)
             in zipWith newVar ws [0::Integer ..]
        lf = LambdaForm vs NoUpdate [] (AppC con (map AtomVar vs))
        heap' = H.update addrU (Closure lf ws) heap

    in s { stgCode        = ReturnCon con ws
         , stgArgStack    = argSU
         , stgReturnStack = retSU
         , stgUpdateStack = updS'
         , stgHeap        = heap'
         , stgInfo        = Info (StateTransiton ReturnCon_Update)
            [ "Trying to return " <> prettyprint con <> " without anything on argument/return stacks"
            , "Update closure at " <> prettyprint addrU <> " with returned constructor"
            , "Restore argument/return stacks from the update frame" ] }



-- (17a) Enter partially applied closure
stgRule s@StgState
    { stgCode        = Enter addr
    , stgArgStack    = argS
    , stgReturnStack = Empty
    , stgUpdateStack = UpdateFrame argSU retSU addrU :< updS'
    , stgHeap        = heap
    , stgTicks       = ticks }
    | Just (Closure (LambdaForm _vs NoUpdate xs _body) _wsf) <- H.lookup addr heap
    , F.length argS < L.length xs

  = let argS' = argS <> argSU
        (xs1, _xs2) = splitAt (F.length argS) xs
        f = Var ("upd17a_" <> show' ticks)
        fxs1 = AppF f (map AtomVar xs1)
        moreArgsClosure = Closure (LambdaForm (f : xs1) NoUpdate [] fxs1)
                                  (Addr addr : F.foldMap (\(ArgumentFrame v) -> [v]) argS)
        heap' = H.update addrU moreArgsClosure heap

    in s { stgCode        = Enter addr
         , stgArgStack    = argS'
         , stgReturnStack = retSU
         , stgUpdateStack = updS'
         , stgHeap        = heap'
         , stgInfo        = Info (StateTransiton Enter_PartiallyAppliedUpdate) [] }

stgRule s = noRuleApplies s



-- | When there are no rules, the machine halts. But there are many different
-- ways this state can be reached, so it's helpful to the user to distinguish
-- them from each other.
noRuleApplies :: StgState -> StgState

-- Page 39, 2nd paragraph: "[...] closures with non-emptyargument lists are
-- never updatable [...]"
noRuleApplies s@StgState
    { stgCode = Enter addr
    , stgHeap = heap }
    | Just (Closure (LambdaForm _ Update (_:_) _) _) <- H.lookup addr heap
  = s { stgInfo = Info (StateError UpdatableClosureWithArgs) [] }



-- Page 39, 4th paragraph: "It is not possible for the ReturnInt state to see an
-- empty return stack, because that would imply that a closure should be updated
-- with a primitive value; but no closure has a primitive type.
noRuleApplies s@StgState
    { stgCode        = ReturnInt{}
    , stgUpdateStack = Empty }
  = s { stgInfo = Info (StateError ReturnIntWithEmptyReturnStack)
        ["No closure has primitive type, so we cannot update one with a primitive int"] }



-- Function argument not in scope
noRuleApplies s@StgState
    { stgCode    = Eval (AppF f xs) locals
    , stgGlobals = globals }
    | Failure (NotInScope notInScope) <- vals locals globals (AtomVar f : xs)
  = s { stgInfo = Info (StateError (VariablesNotInScope notInScope)) [] }

-- Constructor argument not in scope
noRuleApplies s@StgState
    { stgCode    = Eval (AppC _con xs) locals
    , stgGlobals = globals }
    | Failure (NotInScope notInScope) <- vals locals globals xs
  = s { stgInfo = Info (StateError (VariablesNotInScope notInScope)) [] }



-- Algebraic constructor return, but primitive alternative on return frame
noRuleApplies s@StgState
    { stgCode        = ReturnCon{}
    , stgReturnStack = ReturnFrame (Alts (AlgebraicAlt{}:_) _) _ :< _ }
  = s { stgInfo = Info (StateError AlgReturnToPrimAlts) [] }

-- Primitive return, but algebraic alternative on return frame
noRuleApplies s@StgState
    { stgCode        = ReturnInt _
    , stgReturnStack = ReturnFrame (Alts (PrimitiveAlt{}:_) _) _ :< _ }
  = s { stgInfo = Info (StateError PrimReturnToAlgAlts) [] }



-- Successful, ordinary termination
noRuleApplies s@StgState
    { stgArgStack    = S.Empty
    , stgReturnStack = S.Empty
    , stgUpdateStack = S.Empty }
  = s { stgInfo = Info NoRulesApply [] }



noRuleApplies s = s { stgInfo = Info NoRulesApply
    [ "Stacks are not empty; the program terminated unexpectedly."
    , "The lack of a better description is a bug in the STG evaluator."
    , "Please report this to the project maintainers!" ] }
