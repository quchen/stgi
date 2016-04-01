{-# LANGUAGE LambdaCase #-}

module Lib where



import           Control.Applicative
import           Control.Monad
import qualified Data.List           as L
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Monoid
import           GHC.Exts            (fromList)

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

unionLocals :: Locals -> Locals -> Locals
unionLocals (Locals l1) (Locals l2) = Locals (M.union l1 l2)

val :: Locals -> Globals -> Atom -> Maybe Value
val (Locals locals) (Globals globals) = \case
    AtomLit (Literal k) -> Just (PrimInt k)
    AtomVar v           -> M.lookup v locals <|> M.lookup v globals

vals :: Locals -> Globals -> [Atom] -> Maybe [Value]
vals locals globals = traverse (val locals globals)

data StgState = StgState
    { stgCode        :: Code
    , stgArgStack    :: Stack Value
    , stgReturnStack :: Stack (Alts, Locals)
    , stgUpdateStack :: Stack (Stack Value, Stack (Alts, Locals), MemAddr)
    , stgHeap        :: Heap
    , stgGlobals     :: Globals
    , ticks          :: Integer }

initialState :: Expr -> Globals -> StgState
initialState main' globals = StgState
    { stgCode        = Eval main' (Locals M.empty)
    , stgArgStack    = mempty
    , stgReturnStack = mempty
    , stgUpdateStack = mempty
    , stgHeap        = Heap M.empty
    , stgGlobals     = globals
    , ticks          = 0 }

stgStep :: StgState -> StgState

-- (1) DONE
stgStep (StgState (Eval (AppF f xs) locals) argS retS updS heap globals ticks)
    | Just (Addr a) <- val locals globals (AtomVar f)
  = do
    xsVals <- vals locals globals xs
    let argS' = xsVals <>> argS
    pure (StgState (Enter a) argS' retS updS heap globals ticks)

-- (2)
stgStep (StgState (Enter a) argS retS updS heap globals ticks)
    | Just (Closure (LambdaForm free update bound body) freeVals) <- heapLookup a heap
    , Just (args, argS') <- S.popN (length bound) argS
  = Right (
    let locals = addLocals (zip free freeVals ++ zip bound args) emptyLocals
    in StgState (Eval body locals) argS' retS updS heap globals ticks)

-- (3)
stgStep (StgState (Eval (Let rec binds expr) locals) argS retS updS heap globals ticks)
  = Right (StgState (Eval expr locals') argS retS updS heap globals ticks)
  where
    locals' = undefined

    localsRhs = case rec of
        NonRecursive -> locals
        Recursive    -> locals'

-- (4) DONE
stgStep (StgState (Eval (Case expr alts) locals) argS retS updS heap globals ticks)
  = let retS' = (alts, locals) :< retS
    in Right (StgState (Eval expr locals) argS retS updS heap globals ticks)

-- (5) DONE
stgStep (StgState (Eval (AppC con xs) locals) argS retS updS heap globals ticks) = do
    valsXs <- vals locals globals xs
    Right (StgState (ReturnCon con valsXs) argS retS updS heap globals ticks)

-- (6)
stgStep (StgState (ReturnCon con ws) argS retS updS heap globals ticks)
    | (alts@(AlgebraicAlts _ _), locals) :< retS' <- retS
    , Left (Right (AAlt _con vars expr)) <- lookupAlts alts con
  = let locals' = undefined
    in Right (StgState (Eval expr locals) argS retS' updS heap globals ticks)

-- (7)

-- (8)

-- (9) DONE
stgStep (StgState (Eval (Lit (Literal k)) _locals) argS retS updS heap globals ticks)
  = Right (StgState (ReturnInt k) argS retS updS heap globals ticks)

-- (10) DONE
stgStep (StgState (Eval (AppF f []) locals) argS retS updS heap globals ticks)
    | Right (PrimInt k) <- val locals emptyGlobals (AtomVar f)
  = Right (StgState (ReturnInt k) argS retS updS heap globals ticks)

-- (11)

-- (12)

-- (13)

-- (14)

-- (15)
stgStep (StgState (Enter addr) argS retS updS heap globals ticks)
    | Just (LambdaForm vs Update [] body) <- heapLookup addr heap
  = let updS' = (argS, retS, addr) :< updS
        -- addLocals :: [(Var, MemAddr)] -> Locals -> Locals
        locals = addLocals (zip undefined undefined) emptyLocals
    in Right (StgState (Eval body locals) Empty Empty updS' heap globals ticks)

-- (16)
stgStep (StgState (ReturnCon con ws) Empty Empty ((argSU, retSU, addrU) :< updS') heap globals ticks) = do
    let vs = let newVar _old i = Var ("Var:tick " ++ show ticks ++ "#" ++ show i)
             in zipWith newVar ws [0..]
        lf = LambdaForm vs NoUpdate [] (AppC con (map AtomVar vs))
        heap' = heapUpdate addrU lf heap
        ticks' = ticks+1
    Right (StgState (ReturnCon con ws) argSU retSU updS' heap' globals ticks')

-- (17)
stgStep (StgState (Enter addr) argS Empty ((argSU, retSU, addrU) :< updS') heap globals ticks) = do
    let argS' = argS <> argSU
    undefined

-- (17a)
