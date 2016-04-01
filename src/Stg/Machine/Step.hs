{-# LANGUAGE LambdaCase #-}

module Stg.Machine.Step (
    initialState,
    stgStep,
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



data StgState = StgState
    { stgCode        :: Code
    , stgArgStack    :: Stack Value
    , stgReturnStack :: Stack (Alts, Locals)
    , stgUpdateStack :: Stack (Stack Value, Stack (Alts, Locals), MemAddr)
    , stgHeap        :: Heap
    , stgGlobals     :: Globals
    , stgTicks       :: Integer }

initialState
    :: Var -- ^ Main
    -> Binds
    -> StgState
initialState mainVar binds = StgState
    { stgCode        = Eval (AppF mainVar []) mempty
    , stgArgStack    = mempty
    , stgReturnStack = mempty
    , stgUpdateStack = mempty
    , stgHeap        = heap
    , stgGlobals     = globals
    , stgTicks       = 0 }
  where
    globalVars :: [Var]
    globalVals :: [LambdaForm]
    (globalVars, globalVals) = let Binds b = binds in unzip (M.assocs b)

    globals :: Globals
    globals = makeGlobals (zipWith (\n a -> (n, Addr a)) globalVars addrs)

    addrs :: [MemAddr]
    heap :: Heap
    (addrs, heap) = H.allocMany (map liftClosure globalVals) mempty

    -- TODO: Unify this with the other liftClosure
    liftClosure :: LambdaForm -> Closure
    liftClosure lf@(LambdaForm free _ _ _) =
        let freeVals :: [Value]
            freeVals = fromMaybe (error "FOOBAR-MAIN")
                                 (traverse (globalVal globals) free)
        in Closure lf freeVals

-- | Look up an algebraic constructor among the given alternatives, and return
-- the first match. If nothing matches, return the default alternative.
lookupAAlts :: AAlts -> Constr -> Either DefaultAlt AAlt
lookupAAlts (AAlts alts def) constr
    | Just alt <- L.find (\(AAlt c _ _) -> c == constr) alts = Right alt
    | otherwise = Left def

-- | 'lookupAAlts' for primitive literals.
lookupPAlts :: PAlts -> Literal -> Either DefaultAlt PAlt
lookupPAlts (PAlts alts def) lit
    | Just alt <- L.find (\(PAlt lit' _) -> lit' == lit) alts = Right alt
    | otherwise = Left def

-- | Perform a single STG machine step.
stgStep :: StgState -> StgState

-- (1)
-- TODO: Test (1)
stgStep s@StgState
    { stgCode     = Eval (AppF f xs) locals
    , stgArgStack = argS
    , stgGlobals  = globals }
    | Just (Addr a) <- val locals globals (AtomVar f)

  = let xsVals = unsafeVals locals globals xs
        argS' = xsVals <>> argS

    in s { stgCode     = Enter a
         , stgArgStack = argS' }

-- (2)
-- TODO: Test (2)
stgStep s@StgState
    { stgCode     = Enter a
    , stgArgStack = argS
    , stgHeap     = heap }
    | Just (Closure (LambdaForm free NoUpdate bound body) freeVals) <- H.lookup a heap
    , Just (args, argS') <- S.popN (length bound) argS

  = let locals = makeLocals (zip free freeVals ++ zip bound args)

    in s { stgCode     = Eval body locals
         , stgArgStack = argS' }

-- (3)
-- TODO: Test (3)
stgStep s@StgState
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




-- (4)
-- TODO: Test (4)
stgStep s@StgState
    { stgCode        = (Eval (Case expr alts) locals)
    , stgReturnStack = retS }

  = let retS' = (alts, locals) :< retS

    in s { stgCode        = Eval expr locals
         , stgReturnStack = retS'  }

-- (5)
-- TODO: Test (5)
stgStep s@StgState
    { stgCode    = Eval (AppC con xs) locals
    , stgGlobals = globals }

  = let valsXs = unsafeVals locals globals xs

    in s { stgCode = ReturnCon con valsXs }

-- (6)
-- TODO: Test (6)
stgStep s@StgState
    { stgCode        = ReturnCon con ws
    , stgReturnStack = (AlgebraicAlts alts, locals) :< retS' }
    | Right (AAlt _con vars expr) <- lookupAAlts alts con

  = let locals' = addLocals (zip vars ws) locals

    in s { stgCode        = Eval expr locals'
         , stgReturnStack = retS' }

-- (7)
-- TODO: Test (7)
stgStep s@StgState
    { stgCode        = ReturnCon con _ws
    , stgReturnStack = (AlgebraicAlts alts, locals) :< retS' }
    | Left (DefaultNotBound expr) <- lookupAAlts alts con

  = s { stgCode        = Eval expr locals
      , stgReturnStack = retS' }

-- (8)
-- TODO: Test (8)
stgStep s@StgState
    { stgCode        = ReturnCon con ws
    , stgReturnStack = (AlgebraicAlts alts, locals) :< retS'
    , stgHeap        = heap
    , stgTicks       = ticks }
    | Left (DefaultBound (AtomVar v) expr) <- lookupAAlts alts con

  = let locals' = addLocals [(v, Addr addr)] locals
        (addr, heap') = H.alloc closure heap
        closure = Closure (LambdaForm vs NoUpdate [] (AppC con (map AtomVar vs))) ws
        vs = let newVar _old i = Var ("Var/Def:tick " ++ show ticks ++ "#" ++ show i)
             in zipWith newVar ws [0::Integer ..]
    in s { stgCode        = Eval expr locals'
         , stgReturnStack = retS'
         , stgHeap        = heap'
         , stgTicks       = ticks+1 }

-- (9)
-- TODO: Test (9)
stgStep s@StgState { stgCode = Eval (Lit (Literal k)) _locals}
  = s { stgCode = ReturnInt k }

-- (10)
-- TODO: Test (10)
stgStep s@StgState { stgCode = Eval (AppF f []) locals}
    | Just (PrimInt k) <- val locals mempty (AtomVar f)

  = s { stgCode = ReturnInt k }

-- (11)
-- TODO: Test (11)
stgStep s@StgState
    { stgCode        = ReturnInt k
    , stgReturnStack = (PrimitiveAlts alts, locals) :< retS' }
    | Right (PAlt _k expr) <- lookupPAlts alts (Literal k)

  = s { stgCode        = Eval expr locals
      , stgReturnStack = retS' }

-- (12)
stgStep s@StgState
    { stgCode        = ReturnInt k
    , stgReturnStack = (PrimitiveAlts alts, locals) :< retS' }
    | Left (DefaultBound (AtomVar v) expr) <- lookupPAlts alts (Literal k)

  = let locals' = addLocals [(v, PrimInt k)] locals

    in s { stgCode        = Eval expr locals'
         , stgReturnStack = retS' }

-- (13)
-- TODO: Test (13)
stgStep s@StgState
    { stgCode        = ReturnInt k
    , stgReturnStack = (PrimitiveAlts alts, locals) :< retS' }
    | Left (DefaultNotBound expr) <- lookupPAlts alts (Literal k)

  = s { stgCode        = Eval expr locals
      , stgReturnStack = retS' }

-- (14)
-- TODO: Test (14)
stgStep s@StgState
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

-- (15)
stgStep s@StgState
    { stgCode        = Enter addr
    , stgArgStack    = argS
    , stgReturnStack = retS
    , stgUpdateStack = updS
    , stgHeap        = heap }
    | Just (Closure (LambdaForm free Update [] body) freeVals) <- H.lookup addr heap
        -- TODO: Is the closure removed from the heap?

  = let updS' = (argS, retS, addr) :< updS
        locals = makeLocals (zip free freeVals)

    in s { stgCode        = Eval body locals
         , stgArgStack    = Empty
         , stgReturnStack = Empty
         , stgUpdateStack = updS' }

-- (16)
-- TODO: Test (16)
stgStep s@StgState
    { stgCode        = ReturnCon con ws
    , stgArgStack    = Empty
    , stgReturnStack = Empty
    , stgUpdateStack = (argSU, retSU, addrU) :< updS'
    , stgHeap        = heap
    , stgTicks       = ticks }

  = let vs = let newVar _old i = Var ("Var/Upd1:tick " ++ show ticks ++ "#" ++ show i)
             in zipWith newVar ws [0::Integer ..]
        lf = LambdaForm vs NoUpdate [] (AppC con (map AtomVar vs))
        heap' = H.update addrU (Closure lf ws) heap

    in s { stgCode        = ReturnCon con ws
         , stgArgStack    = argSU
         , stgReturnStack = retSU
         , stgUpdateStack = updS'
         , stgHeap        = heap'
         , stgTicks       = ticks+1 }

-- (17a)
-- TODO: Test (17a)
stgStep s@StgState
    { stgCode        = Enter addr
    , stgArgStack    = argS
    , stgReturnStack = Empty
    , stgUpdateStack = (argSU, retSU, addrU) :< updS'
    , stgHeap        = heap
    , stgTicks       = ticks }
    | Just (Closure (LambdaForm _vs NoUpdate xs body) _wsf) <- H.lookup addr heap
    , S.size argS < L.length xs

  = let argS' = argS <> argSU
        (xs1, xs2) = splitAt (S.size argS) xs
        f = Var ("Var/Upd2:tick " ++ show ticks)
        moreArgsClosure = Closure (LambdaForm (f : xs1) NoUpdate xs2 body)
                                  (Addr addr : F.toList argS)
        heap' = H.update addrU moreArgsClosure heap

    in s { stgCode        = Enter addr
         , stgArgStack    = argS'
         , stgReturnStack = retSU
         , stgUpdateStack = updS'
         , stgHeap        = heap'
         , stgTicks       = ticks+1}
