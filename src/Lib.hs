{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Lib where



import           Control.Applicative
import qualified Data.Foldable       as F
import qualified Data.List           as L
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Monoid

import           Stack               (Stack (..), (<>>))
import qualified Stack               as S
import           StgLanguage



data Closure = Closure LambdaForm [Value]

newtype Heap = Heap (Map MemAddr Closure)

heapLookup :: MemAddr -> Heap -> Maybe Closure
heapLookup addr (Heap heap) = M.lookup addr heap

heapUpdate :: MemAddr -> Closure -> Heap -> Heap
heapUpdate addr cl (Heap h) = Heap (M.adjust (const cl) addr h)

heapAlloc :: Closure -> Heap -> (MemAddr, Heap)
heapAlloc lambdaForm (Heap h) = (addr, heap')
  where
    addr = MemAddr (case M.maxViewWithKey h of
        Just ((MemAddr x,_),_) -> x + 1
        Nothing                -> 0 )
    heap' = Heap (M.insert addr lambdaForm h)

heapAllocMany :: [Closure] -> Heap -> ([MemAddr], Heap)
heapAllocMany [] heap = ([], heap)
heapAllocMany (cl:cls) heap =
    let (addr, heap') = heapAlloc cl heap
        (addrs, heap'') = heapAllocMany cls heap'
    in (addr:addrs, heap'')

lookupAAlts :: [AAlt] -> DefaultAlt -> Constr -> Either DefaultAlt AAlt
lookupAAlts aAlts def constr
    | Just alt <- L.find (\(AAlt c _ _) -> c == constr) aAlts = Right alt
    | otherwise = Left def

lookupPAlts :: [PAlt] -> DefaultAlt -> Literal -> Either DefaultAlt PAlt
lookupPAlts pAlts def lit
    | Just alt <- L.find (\(PAlt lit' _) -> lit' == lit) pAlts = Right alt
    | otherwise = Left def


newtype MemAddr = MemAddr Int deriving (Eq, Ord, Show)
data Value = Addr MemAddr | PrimInt Int
data Code = Eval Expr Locals
          | Enter MemAddr
          | ReturnCon Constr [Value]
          | ReturnInt Int
newtype Globals = Globals (Map Var Value) deriving (Monoid)
newtype Locals = Locals (Map Var Value) deriving (Monoid)

addLocal :: (Var, Value) -> Locals -> Locals
addLocal (var, addr) (Locals locals) = Locals (M.insert var addr locals)

addLocals :: [(Var, Value)] -> Locals -> Locals
addLocals defs locals = foldr addLocal locals defs

makeLocals :: [(Var, Value)] -> Locals
makeLocals defs = addLocals defs mempty

val :: Locals -> Globals -> Atom -> Maybe Value
val (Locals locals) (Globals globals) = \case
    AtomLit (Literal k) -> Just (PrimInt k)
    AtomVar v           -> M.lookup v locals <|> M.lookup v globals

vals :: Locals -> Globals -> [Atom] -> Maybe [Value]
vals locals globals = traverse (val locals globals)

unsafeVals :: Locals -> Globals -> [Atom] -> [Value]
unsafeVals l g a = fromMaybe (error "Variable not found") (vals l g a)

localVal :: Locals -> Var -> Maybe Value
localVal (Locals locals) var = M.lookup var locals

data StgState = StgState
    { stgCode        :: Code
    , stgArgStack    :: Stack Value
    , stgReturnStack :: Stack (Alts, Locals)
    , stgUpdateStack :: Stack (Stack Value, Stack (Alts, Locals), MemAddr)
    , stgHeap        :: Heap
    , stgGlobals     :: Globals
    , stgTicks       :: Integer }

initialState :: Expr -> Globals -> StgState
initialState main' globals = StgState
    { stgCode        = Eval main' (Locals M.empty)
    , stgArgStack    = mempty
    , stgReturnStack = mempty
    , stgUpdateStack = mempty
    , stgHeap        = Heap M.empty
    , stgGlobals     = globals
    , stgTicks       = 0 }

stgStep :: StgState -> StgState

-- (1) DONE
stgStep s@StgState
    { stgCode     = Eval (AppF f xs) locals
    , stgArgStack = argS
    , stgGlobals  = globals }
    | Just (Addr a) <- val locals globals (AtomVar f)

  = let xsVals = unsafeVals locals globals xs
        argS' = xsVals <>> argS

    in s { stgCode     = Enter a
         , stgArgStack = argS' }

-- (2) DONE
stgStep s@StgState
    { stgCode     = Enter a
    , stgArgStack = argS
    , stgHeap     = heap }
    | Just (Closure (LambdaForm free NoUpdate bound body) freeVals) <- heapLookup a heap
    , Just (args, argS') <- S.popN (length bound) argS

  = let locals = makeLocals (zip free freeVals ++ zip bound args)

    in s { stgCode     = Eval body locals
         , stgArgStack = argS' }

-- (3) DONE
stgStep s@StgState
    { stgCode = Eval (Let rec (Binds binds) expr) locals
    , stgHeap = heap }

  = let locals' = makeLocals (zipWith (\n a -> (n, Addr a))
                             (M.keys binds)
                             addrs )

        (addrs, heap') = heapAllocMany (map liftClosure (M.elems binds)) heap

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




-- (4) DONE
stgStep s@StgState
    { stgCode        = (Eval (Case expr alts) locals)
    , stgReturnStack = retS }

  = let retS' = (alts, locals) :< retS

    in s { stgCode        = Eval expr locals
         , stgReturnStack = retS'  }

-- (5) DONE
stgStep s@StgState
    { stgCode    = Eval (AppC con xs) locals
    , stgGlobals = globals }

  = let valsXs = unsafeVals locals globals xs

    in s { stgCode = ReturnCon con valsXs }

-- (6) DONE
stgStep s@StgState
    { stgCode        = ReturnCon con ws
    , stgReturnStack = (AlgebraicAlts alts def, locals) :< retS' }
    | Right (AAlt _con vars expr) <- lookupAAlts alts def con

  = let locals' = addLocals (zip vars ws) locals

    in s { stgCode        = Eval expr locals'
         , stgReturnStack = retS' }

-- (7) DONE
stgStep s@StgState
    { stgCode        = ReturnCon con _ws
    , stgReturnStack = (AlgebraicAlts alts def, locals) :< retS' }
    | Left (DefaultNotBound expr) <- lookupAAlts alts def con

  = s { stgCode        = Eval expr locals
      , stgReturnStack = retS' }

-- (8) DONE
stgStep s@StgState
    { stgCode        = ReturnCon con ws
    , stgReturnStack = (AlgebraicAlts alts def, locals) :< retS'
    , stgHeap        = heap
    , stgTicks       = ticks }
    | Left (DefaultBound (AtomVar v) expr) <- lookupAAlts alts def con

  = let locals' = addLocal (v, Addr addr) locals
        (addr, heap') = heapAlloc closure heap
        closure = Closure (LambdaForm vs NoUpdate [] (AppC con (map AtomVar vs))) ws
        vs = let newVar _old i = Var ("Var/Def:tick " ++ show ticks ++ "#" ++ show i)
             in zipWith newVar ws [0::Integer ..]
    in s { stgCode        = Eval expr locals'
         , stgReturnStack = retS'
         , stgHeap        = heap'
         , stgTicks       = ticks+1 }

-- (9) DONE
stgStep s@StgState { stgCode = Eval (Lit (Literal k)) _locals}
  = s { stgCode = ReturnInt k }

-- (10) DONE
stgStep s@StgState { stgCode = Eval (AppF f []) locals}
    | Just (PrimInt k) <- val locals mempty (AtomVar f)

  = s { stgCode = ReturnInt k }

-- (11) DONE
stgStep s@StgState
    { stgCode        = ReturnInt k
    , stgReturnStack = (PrimitiveAlts alts def, locals) :< retS' }
    | Right (PAlt _k expr) <- lookupPAlts alts def (Literal k)

  = s { stgCode        = Eval expr locals
      , stgReturnStack = retS' }

-- (12)
stgStep s@StgState
    { stgCode        = ReturnInt k
    , stgReturnStack = (PrimitiveAlts alts def, locals) :< retS' }
    | Left (DefaultBound (AtomVar v) expr) <- lookupPAlts alts def (Literal k)

  = let locals' = addLocal (v, PrimInt k) locals

    in s { stgCode        = Eval expr locals'
         , stgReturnStack = retS' }

-- (13) DONE
stgStep s@StgState
    { stgCode        = ReturnInt k
    , stgReturnStack = (PrimitiveAlts alts def, locals) :< retS' }
    | Left (DefaultNotBound expr) <- lookupPAlts alts def (Literal k)

  = s { stgCode        = Eval expr locals
      , stgReturnStack = retS' }

-- (14) DONE
stgStep s@StgState
    { stgCode = Eval (AppP op [AtomVar x, AtomVar y]) locals }
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
    | Just (Closure (LambdaForm free Update [] body) freeVals) <- heapLookup addr heap
        -- TODO: Is the closure removed from the heap?

  = let updS' = (argS, retS, addr) :< updS
        locals = makeLocals (zip free freeVals)

    in s { stgCode        = Eval body locals
         , stgArgStack    = Empty
         , stgReturnStack = Empty
         , stgUpdateStack = updS' }

-- (16) DONE
stgStep s@StgState
    { stgCode        = ReturnCon con ws
    , stgArgStack    = Empty
    , stgReturnStack = Empty
    , stgUpdateStack = (argSU, retSU, addrU) :< updS'
    , stgHeap        = heap
    , stgTicks       = ticks }

  = let vs = let newVar _old i = Var ("Var/Upd:tick " ++ show ticks ++ "#" ++ show i)
             in zipWith newVar ws [0::Integer ..]
        lf = LambdaForm vs NoUpdate [] (AppC con (map AtomVar vs))
        heap' = heapUpdate addrU (Closure lf ws) heap

    in s { stgCode        = ReturnCon con ws
         , stgArgStack    = argSU
         , stgReturnStack = retSU
         , stgUpdateStack = updS'
         , stgHeap        = heap'
         , stgTicks       = ticks+1 }

-- (17) DONE
stgStep s@StgState
    { stgCode        = Enter addr
    , stgArgStack    = argS
    , stgReturnStack = Empty
    , stgUpdateStack = (argSU, retSU, addrU) :< updS'
    , stgHeap        = heap }
    | Just (Closure (LambdaForm vs NoUpdate xs body) wsf) <- heapLookup addr heap
    , S.size argS < L.length xs

  = let argS' = argS <> argSU
        (xs1, xs2) = splitAt (S.size argS) xs
        moreArgsClosure = Closure (LambdaForm (vs <> xs1) NoUpdate xs2 body)
                                  (wsf <> F.toList argS)
        heap' = heapUpdate addrU moreArgsClosure heap

    in s { stgCode        = Enter addr
         , stgArgStack    = argS'
         , stgReturnStack = retSU
         , stgUpdateStack = updS'
         , stgHeap        = heap' }

-- (17a)
