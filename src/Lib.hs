{-# LANGUAGE LambdaCase #-}

module Lib where



import           Control.Applicative
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

lookupAlts
    :: Alts
    -> Constr
    -> Either (Either DefaultAlt AAlt) (Either DefaultAlt PAlt)
lookupAlts (AlgebraicAlts aalts def) constr
    | Just alt <- L.find (\(AAlt c _ _) -> c == constr) aalts = Left (Right alt)
    | otherwise = Left (Left def)


newtype MemAddr = MemAddr Int deriving (Eq, Ord, Show)
data Value = Addr MemAddr | PrimInt Int
data Code = Eval Expr Locals
          | Enter MemAddr
          | ReturnCon Constr [Value]
          | ReturnInt Int
newtype Globals = Globals (Map Var Value)
newtype Locals = Locals (Map Var Value)

emptyGlobals :: Globals
emptyGlobals = Globals M.empty

emptyLocals :: Locals
emptyLocals = Locals M.empty

addLocal :: (Var, Value) -> Locals -> Locals
addLocal (var, addr) (Locals locals) = Locals (M.insert var addr locals)

addLocals :: [(Var, Value)] -> Locals -> Locals
addLocals defs locals = foldr addLocal locals defs

makeLocals :: [(Var, Value)] -> Locals
makeLocals defs = addLocals defs emptyLocals

unionLocals :: Locals -> Locals -> Locals
unionLocals (Locals l1) (Locals l2) = Locals (M.union l1 l2)

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

-- (6)
stgStep s@StgState
    { stgCode        = ReturnCon con ws
    , stgReturnStack = retS }
    | (alts@(AlgebraicAlts _ _), locals) :< retS' <- retS
    , Left (Right (AAlt _con vars expr)) <- lookupAlts alts con

  = let locals' = undefined
        _hideUnused = undefined locals' vars ws

    in s { stgCode        = Eval expr locals
         , stgReturnStack = retS' }

-- (7)

-- (8)

-- (9) DONE
stgStep s@StgState { stgCode = Eval (Lit (Literal k)) _locals}
  = s { stgCode = ReturnInt k }

-- (10) DONE
stgStep s@StgState { stgCode = Eval (AppF f []) locals}
    | Just (PrimInt k) <- val locals emptyGlobals (AtomVar f)

  = s { stgCode = ReturnInt k }

-- (11)

-- (12)

-- (13)

-- (14)

-- (15)
stgStep s@StgState
    { stgCode        = Enter addr
    , stgArgStack    = argS
    , stgReturnStack = retS
    , stgUpdateStack = updS
    , stgHeap        = heap
    , stgGlobals     = globals
    , stgTicks       = ticks }
    | Just (Closure (LambdaForm free Update [] body) freeVals) <- heapLookup addr heap

  = let updS' = (argS, retS, addr) :< updS
        -- addLocals :: [(Var, MemAddr)] -> Locals -> Locals
        locals = makeLocals (zip undefined undefined)
        _ignoreUnused = undefined globals ticks free freeVals

    in s { stgCode        = Eval body locals
         , stgArgStack    = Empty
         , stgReturnStack = Empty
         , stgUpdateStack = updS' }

-- (16)
stgStep s@StgState
    { stgCode        = ReturnCon con ws
    , stgArgStack    = Empty
    , stgReturnStack = Empty
    , stgUpdateStack = (argSU, retSU, addrU) :< updS'
    , stgHeap        = heap
    , stgTicks       = ticks }

  = let vs = let newVar _old i = Var ("Var:tick " ++ show ticks ++ "#" ++ show i)
             in zipWith newVar ws [0..]
        lf = LambdaForm vs NoUpdate [] (AppC con (map AtomVar vs))
        heap' = heapUpdate addrU (Closure lf ws) heap
        ticks' = ticks+1

    in s { stgCode        = ReturnCon con ws
         , stgArgStack    = argSU
         , stgReturnStack = retSU
         , stgUpdateStack = updS'
         , stgHeap        = heap'
         , stgTicks       = ticks' }

-- (17)
stgStep s@StgState
    { stgCode        = Enter addr
    , stgArgStack    = argS
    , stgReturnStack = Empty
    , stgUpdateStack = (argSU, retSU, addrU) :< updS'
    , stgHeap        = heap
    , stgGlobals     = globals
    , stgTicks       = ticks }

  = let argS' = argS <> argSU

    in undefined s addr retSU addrU updS' heap globals ticks argS'

-- (17a)
