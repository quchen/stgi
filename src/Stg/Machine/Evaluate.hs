{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Stg.Machine.Evaluate (
    evalStep,
    evalUntil,
    evalUntil',
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
lookupAlgebraicAlts :: AlgebraicAlts -> Constr -> Either DefaultAlt AlgebraicAlt
lookupAlgebraicAlts (AlgebraicAlts alts def) constr
    | Just alt <- L.find (\(AlgebraicAlt c _ _) -> c == constr) alts = Right alt
    | otherwise = Left def

-- | 'lookupAAlts' for primitive literals.
lookupPrimitiveAlts :: PrimitiveAlts -> Literal -> Either DefaultAlt PrimitiveAlt
lookupPrimitiveAlts (PrimitiveAlts alts def) lit
    | Just alt <- L.find (\(PrimitiveAlt lit' _) -> lit' == lit) alts = Right alt
    | otherwise = Left def

-- | Perform a single STG machine evaluation step.
evalStep :: StgState -> StgState
evalStep state = let state' = stgRule state
                 in state' { stgTicks = stgTicks state' + 1 }

-- | Evaluate the STG until a predicate holds.
--
-- A safer version of this is 'evalUntil', which aborts if a certain
-- maximum number of steps is exceeded.
evalUntil'
    :: (StgState -> Bool) -- ^ Halting decision function
    -> StgState           -- ^ Initial state
    -> StgState           -- ^ Final state
evalUntil' p = until p evalStep

-- | Evaluate the STG until a predicate holds, aborting if the maximum number of
-- steps are exceeded.
--
-- Use 'evalUntil\'' if you do not want a step limit.
evalUntil
    :: Integer                  -- ^ Maximum number of steps allowed
    -> (StgState -> Bool)       -- ^ Halting decision function
    -> StgState                 -- ^ Initial state
    -> Either StgState StgState -- ^ Final state. 'Left' if the maximum
                                --   number of steps are exceeded.
evalUntil maxSteps p state
    | stgTicks state > maxSteps = Left state
    | p state                   = Right state
    | otherwise                 = evalUntil maxSteps p (evalStep state)



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
         , stgArgStack = argS' }

-- (2) Entering non-updatable closures
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
         , stgArgStack = argS' }

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

    in s { stgCode = Eval expr locals'
         , stgHeap = heap' }

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

    in s { stgCode = ReturnCon con valsXs }

-- (6) Algebraic constructor return, standard match found
stgRule s@StgState
    { stgCode        = ReturnCon con ws
    , stgReturnStack = ReturnFrame (Algebraic alts) locals :< retS' }
    | Right (AlgebraicAlt _con vars expr) <- lookupAlgebraicAlts alts con

  = let locals' = addLocals (zip vars ws) locals

    in s { stgCode        = Eval expr locals'
         , stgReturnStack = retS' }

-- (7) Algebraic constructor return, unbound default match
stgRule s@StgState
    { stgCode        = ReturnCon con _ws
    , stgReturnStack = ReturnFrame (Algebraic alts) locals :< retS' }
    | Left (DefaultNotBound expr) <- lookupAlgebraicAlts alts con

  = s { stgCode        = Eval expr locals
      , stgReturnStack = retS' }

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
        vs = let newVar _old i = Var ("Var/Def:tick" <> show' ticks <> "#" <> show' i)
             in zipWith newVar ws [0::Integer ..]
    in s { stgCode        = Eval expr locals'
         , stgReturnStack = retS'
         , stgHeap        = heap'
         , stgTicks       = ticks+1 }

-- (9) Literal evaluation
stgRule s@StgState { stgCode = Eval (Lit (Literal k)) _locals}
  = s { stgCode = ReturnInt k }

-- (10) Literal application
stgRule s@StgState { stgCode = Eval (AppF f []) locals }
    | Just (PrimInt k) <- val locals mempty (AtomVar f)

  = s { stgCode = ReturnInt k }

-- (11) Primitive constructor return, standard match found
stgRule s@StgState
    { stgCode        = ReturnInt k
    , stgReturnStack = ReturnFrame (Primitive alts) locals :< retS' }
    | Right (PrimitiveAlt _k expr) <- lookupPrimitiveAlts alts (Literal k)

  = s { stgCode        = Eval expr locals
      , stgReturnStack = retS' }

-- (12) Primitive constructor return, bound default match
stgRule s@StgState
    { stgCode        = ReturnInt k
    , stgReturnStack = ReturnFrame (Primitive alts) locals :< retS' }
    | Left (DefaultBound v expr) <- lookupPrimitiveAlts alts (Literal k)

  = let locals' = addLocals [(v, PrimInt k)] locals

    in s { stgCode        = Eval expr locals'
         , stgReturnStack = retS' }

-- (13) Primitive constructor return, unbound default match
stgRule s@StgState
    { stgCode        = ReturnInt k
    , stgReturnStack = ReturnFrame (Primitive alts) locals :< retS' }
    | Left (DefaultNotBound expr) <- lookupPrimitiveAlts alts (Literal k)

  = s { stgCode        = Eval expr locals
      , stgReturnStack = retS' }

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

    in s { stgCode = ReturnInt (apply op xVal yVal) }

-- (15) Entering updatable closures
stgRule s@StgState
    { stgCode        = Enter addr
    , stgArgStack    = argS
    , stgReturnStack = retS
    , stgUpdateStack = updS
    , stgHeap        = heap }
    | Just (Closure (LambdaForm free Update [] body) freeVals) <- H.lookup addr heap
        -- TODO: Is the closure removed from the heap?

  = let updS' = UpdateFrame argS retS addr :< updS
        locals = makeLocals (zip free freeVals)

    in s { stgCode        = Eval body locals
         , stgArgStack    = Empty
         , stgReturnStack = Empty
         , stgUpdateStack = updS' }

-- (16) Algebraic constructor return, argument stack empty
stgRule s@StgState
    { stgCode        = ReturnCon con ws
    , stgArgStack    = Empty
    , stgReturnStack = Empty
    , stgUpdateStack = UpdateFrame argSU retSU addrU :< updS'
    , stgHeap        = heap
    , stgTicks       = ticks }

  = let vs = let newVar _old i = Var ("Var/Upd1:tick " <> show' ticks <> "#" <> show' i)
             in zipWith newVar ws [0::Integer ..]
        lf = LambdaForm vs NoUpdate [] (AppC con (map AtomVar vs))
        heap' = H.update addrU (Closure lf ws) heap

    in s { stgCode        = ReturnCon con ws
         , stgArgStack    = argSU
         , stgReturnStack = retSU
         , stgUpdateStack = updS'
         , stgHeap        = heap' }

-- (17a) Entering partially applied closures
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
        f = Var ("Var/Upd2:tick " <> show' ticks)
        moreArgsClosure = Closure (LambdaForm (f : xs1) NoUpdate xs2 body)
                                  (Addr addr : F.foldMap (\(ArgumentFrame v) -> [v]) argS)
        heap' = H.update addrU moreArgsClosure heap

    in s { stgCode        = Enter addr
         , stgArgStack    = argS'
         , stgReturnStack = retSU
         , stgUpdateStack = updS'
         , stgHeap        = heap'
         , stgTicks       = ticks+1}

stgRule StgState
    { stgCode        = x@ReturnInt{}
    , stgUpdateStack = Empty }
  = error ("(" <> show x <> ") state with empty update stack")

stgRule _ = error "Invalid STG state"
