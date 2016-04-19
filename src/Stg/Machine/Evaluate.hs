{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stg.Machine.Evaluate (
    evalStep,
    evalUntil,
) where



import qualified Data.Foldable     as F
import qualified Data.List         as L
import qualified Data.Map          as M
import           Data.Maybe
import           Data.Monoid

import           Stack             (Stack (..), (<>>))
import qualified Stack             as S
import           Stg.Language
import           Stg.Machine.Env
import qualified Stg.Machine.Heap  as H
import           Stg.Machine.Types
import           Stg.Util



-- | Look up an algebraic constructor among the given alternatives, and return
-- the first match. If nothing matches, return the default alternative.
lookupAlgebraicAlts
    :: AlgebraicAlts
    -> Constr
    -> Either DefaultAlt AlgebraicAlt
lookupAlgebraicAlts (AlgebraicAlts alts def) constr
  = let matchingAlt (AlgebraicAlt c _ _) = c == constr
    in case L.find matchingAlt alts of
        Just alt -> Right alt
        _otherwise -> Left def

-- | 'lookupAAlts' for primitive literals.
lookupPrimitiveAlts
    :: PrimitiveAlts
    -> Literal
    -> Either DefaultAlt PrimitiveAlt
lookupPrimitiveAlts (PrimitiveAlts alts def) lit
  = let matchingAlt (PrimitiveAlt lit' _) = lit' == lit
    in case L.find matchingAlt alts of
        Just alt -> Right alt
        _otherwise -> Left def

-- | Perform a single STG machine evaluation step.
evalStep :: StgState -> StgState
evalStep state = let state' = stgRule state
                 in state' { stgTicks = stgTicks state' + 1 }

-- | Evaluate the STG until a predicate holds, aborting if the maximum number of
-- steps are exceeded.
evalUntil
    :: Integer            -- ^ Maximum number of steps allowed
    -> (StgState -> Bool) -- ^ Halting decision function
    -> StgState           -- ^ Initial state
    -> StgState           -- ^ Final state
evalUntil maxSteps p = \case
    state@StgState{ stgTicks = ticks } | ticks > maxSteps
        -> state { stgInfo = MaxStepsExceeded }
    state | p state
        -> state { stgInfo = HaltedByPredicate }
    state@StgState{ stgInfo = StateTransiton{} }
        -> evalUntil maxSteps p (evalStep state)
    state
        -> state


-- | Apply a single STG evaluation rule, as specified in the 1992 paper.
stgRule :: StgState -> StgState

-- (1) Function application
stgRule s@StgState
    { stgCode     = Eval (AppF f xs) locals
    , stgArgStack = argS
    , stgGlobals  = globals }
    | Just (Addr a) <- val locals globals (AtomVar f)

  = let xsVals = unsafeVals locals globals xs
        argS' = map ArgumentFrame xsVals <>> argS

    in s { stgCode     = Enter a
         , stgArgStack = argS'
         , stgInfo     = StateTransiton "Function application" }

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
         , stgInfo     = StateTransiton "Enter non-updatable closure" }

-- (3) let(rec)
stgRule s@StgState
    { stgCode = Eval (Let rec (Binds binds) expr) locals
    , stgHeap = heap }

  = let locals' = makeLocals (zipWith (\n a -> (n, Addr a))
                             (M.keys binds)
                             addrs )

        (addrs, heap') = H.allocMany (map liftClosure (M.elems binds)) heap

        liftClosure :: LambdaForm -> Closure
        liftClosure lf@(LambdaForm free _ _ _) =
            let freeVals :: [Value]
                freeVals = fromMaybe (error "FOOBAR")
                                     (traverse (localVal localsRhs) free)
            in Closure lf freeVals

        localsRhs = case rec of
            NonRecursive -> locals
            Recursive    -> locals'

        infotext = case rec of
            NonRecursive -> "let"
            Recursive    -> "letrec"

    in s { stgCode = Eval expr locals'
         , stgHeap = heap'
         , stgInfo = StateTransiton infotext }

-- (4) Case evaluation
stgRule s@StgState
    { stgCode        = (Eval (Case expr alts) locals)
    , stgReturnStack = retS }

  = let retS' = ReturnFrame alts locals :< retS

    in s { stgCode        = Eval expr locals
         , stgReturnStack = retS'  }

-- (5) Constructor application
stgRule s@StgState
    { stgCode    = Eval (AppC con xs) locals
    , stgGlobals = globals }

  = let valsXs = unsafeVals locals globals xs

    in s { stgCode = ReturnCon con valsXs
         , stgInfo = StateTransiton "Constructor application" }

-- (6) Algebraic constructor return, standard match found
stgRule s@StgState
    { stgCode        = ReturnCon con ws
    , stgReturnStack = ReturnFrame (Algebraic alts) locals :< retS' }
    | Right (AlgebraicAlt _con vars expr) <- lookupAlgebraicAlts alts con

  = let locals' = addLocals (zip vars ws) locals

    in s { stgCode        = Eval expr locals'
         , stgReturnStack = retS'
         , stgInfo        = StateTransiton "Algebraic constructor return, standard match" }

-- (7) Algebraic constructor return, unbound default match
stgRule s@StgState
    { stgCode        = ReturnCon con _ws
    , stgReturnStack = ReturnFrame (Algebraic alts) locals :< retS' }
    | Left (DefaultNotBound expr) <- lookupAlgebraicAlts alts con

  = s { stgCode        = Eval expr locals
      , stgReturnStack = retS'
      , stgInfo        = StateTransiton "Algebraic constructor return, unbound default match" }

-- (8) Algebraic constructor return, bound default match
stgRule s@StgState
    { stgCode        = ReturnCon con ws
    , stgReturnStack = ReturnFrame (Algebraic alts) locals :< retS'
    , stgHeap        = heap
    , stgTicks       = ticks }
    | Left (DefaultBound v expr) <- lookupAlgebraicAlts alts con

  = let locals' = addLocals [(v, Addr addr)] locals
        (addr, heap') = H.alloc closure heap
        closure = Closure (LambdaForm vs NoUpdate [] (AppC con (map AtomVar vs))) ws
        vs = let newVar _old i = Var ("alg8_" <> show' ticks <> "#" <> show' i)
             in zipWith newVar ws [0::Integer ..]
    in s { stgCode        = Eval expr locals'
         , stgReturnStack = retS'
         , stgHeap        = heap'
         , stgInfo        = StateTransiton "Algebraic constructor return, bound default match" }

-- (9) Literal evaluation
stgRule s@StgState { stgCode = Eval (Lit (Literal k)) _locals}
  = s { stgCode = ReturnInt k
      , stgInfo = StateTransiton "Literal evaluation" }

-- (10) Literal application
stgRule s@StgState { stgCode = Eval (AppF f []) locals }
    | Just (PrimInt k) <- val locals mempty (AtomVar f)

  = s { stgCode = ReturnInt k
      , stgInfo = StateTransiton "Literal application" }

-- (11) Primitive constructor return, standard match found
stgRule s@StgState
    { stgCode        = ReturnInt k
    , stgReturnStack = ReturnFrame (Primitive alts) locals :< retS' }
    | Right (PrimitiveAlt _k expr) <- lookupPrimitiveAlts alts (Literal k)

  = s { stgCode        = Eval expr locals
      , stgReturnStack = retS'
      , stgInfo        = StateTransiton "Primitive constructor return, standard match found" }

-- (12) Primitive constructor return, bound default match
stgRule s@StgState
    { stgCode        = ReturnInt k
    , stgReturnStack = ReturnFrame (Primitive alts) locals :< retS' }
    | Left (DefaultBound v expr) <- lookupPrimitiveAlts alts (Literal k)

  = let locals' = addLocals [(v, PrimInt k)] locals

    in s { stgCode        = Eval expr locals'
         , stgReturnStack = retS'
         , stgInfo        = StateTransiton "Primitive constructor return, bound default match" }

-- (13) Primitive constructor return, unbound default match
stgRule s@StgState
    { stgCode        = ReturnInt k
    , stgReturnStack = ReturnFrame (Primitive alts) locals :< retS' }
    | Left (DefaultNotBound expr) <- lookupPrimitiveAlts alts (Literal k)

  = s { stgCode        = Eval expr locals
      , stgReturnStack = retS'
      , stgInfo        = StateTransiton "Primitive constructor return, unbound default match" }

-- (14) Primitive function application
stgRule s@StgState
    { stgCode = Eval (AppP op (AtomVar x) (AtomVar y)) locals }
    | Just (PrimInt xVal) <- localVal locals x
    , Just (PrimInt yVal) <- localVal locals y

  = let apply = \case
            Add -> (+)
            Sub -> (-)
            Mul -> (*)
            Div -> quot
            Mod -> rem

    in s { stgCode = ReturnInt (apply op xVal yVal)
         , stgInfo = StateTransiton "Primitive function application"}

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
         , stgInfo        = StateTransiton "Enter updatable closure" }

-- (16) Algebraic constructor return, argument stack empty
stgRule s@StgState
    { stgCode        = ReturnCon con ws
    , stgArgStack    = Empty
    , stgReturnStack = Empty
    , stgUpdateStack = UpdateFrame argSU retSU addrU :< updS'
    , stgHeap        = heap
    , stgTicks       = ticks }

  = let vs = let newVar _old i = Var ("upd16_" <> show' ticks <> "_" <> show' i)
             in zipWith newVar ws [0::Integer ..]
        lf = LambdaForm vs NoUpdate [] (AppC con (map AtomVar vs))
        heap' = H.update addrU (Closure lf ws) heap

    in s { stgCode        = ReturnCon con ws
         , stgArgStack    = argSU
         , stgReturnStack = retSU
         , stgUpdateStack = updS'
         , stgHeap        = heap'
         , stgInfo        = StateTransiton "Algebraic constructor return, argument stack empty" }

-- (17a) Enter partially applied closure
stgRule s@StgState
    { stgCode        = Enter addr
    , stgArgStack    = argS
    , stgReturnStack = Empty
    , stgUpdateStack = UpdateFrame argSU retSU addrU :< updS'
    , stgHeap        = heap
    , stgTicks       = ticks }
    | Just (Closure (LambdaForm _vs NoUpdate xs body) _wsf) <- H.lookup addr heap
    , F.length argS < L.length xs

  = let argS' = argS <> argSU
        (xs1, xs2) = splitAt (F.length argS) xs
        f = Var ("upd17a_" <> show' ticks)
        moreArgsClosure = Closure (LambdaForm (f : xs1) NoUpdate xs2 body)
                                  (Addr addr : F.foldMap (\(ArgumentFrame v) -> [v]) argS)
        heap' = H.update addrU moreArgsClosure heap

    in s { stgCode        = Enter addr
         , stgArgStack    = argS'
         , stgReturnStack = retSU
         , stgUpdateStack = updS'
         , stgHeap        = heap'
         , stgInfo        = StateTransiton "Enter partially applied closure" }

stgRule s@StgState
    { stgCode        = ReturnInt{}
    , stgUpdateStack = Empty }
  = s { stgInfo = StateError "ReturnInt state with empty update stack" }

-- Page 39, 2nd paragraph
stgRule s@StgState
    { stgCode = Enter addr
    , stgHeap = heap }
    | Just (Closure (LambdaForm _ Update (_:_) _) _) <- H.lookup addr heap
  = s { stgInfo = StateError "Closures with non-empty argument lists are never updatable" }

stgRule s = s { stgInfo = NoRulesApply }
