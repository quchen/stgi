{-# LANGUAGE LambdaCase #-}

module Stg where



import           Data.Map    (Map)
import qualified Data.Map    as M

import           Stack
import           StgLanguage



newtype Heap = Heap (Map MemAddr LambdaForm)

heapLookup :: MemAddr -> Heap -> Maybe LambdaForm
heapLookup addr (Heap heap) = M.lookup addr heap

heapAlloc :: LambdaForm -> Heap -> (MemAddr, Heap)
heapAlloc lambdaForm (Heap h) = (addr, heap')
  where
    addr = MemAddr (case M.maxViewWithKey h of
        Just ((MemAddr x,_),_) -> x + 1
        Nothing                -> 0 )
    heap' = Heap (M.insert addr lambdaForm h)

heapAllocMany :: [LambdaForm] -> Heap -> ([MemAddr], Heap)
heapAllocMany [] heap = ([], heap)
heapAllocMany (lf:lfs) heap =
    let (addr, heap') = heapAlloc lf heap
        (addrs, heap'') = heapAllocMany lfs heap'
    in (addr:addrs, heap'')

newtype MemAddr = MemAddr Int deriving (Eq, Ord, Show)
data Value = Addr MemAddr | PrimInt Int
data Code = Eval Expr Locals
          | Enter MemAddr
          | ReturnCon Constr [Value]
          | ReturnInt Int
newtype Globals = Globals (Map Var MemAddr)
newtype Locals = Locals (Map Var MemAddr)

emptyLocals :: Locals
emptyLocals = Locals M.empty

addLocal :: (Var, MemAddr) -> Locals -> Locals
addLocal (var, addr) (Locals locals) = Locals (M.insert var addr locals)

addLocals :: [(Var, MemAddr)] -> Locals -> Locals
addLocals defs locals = foldr addLocal locals defs

unionLocals (Locals l1) (Locals l2) = Locals (M.union l1 l2)

val :: Locals -> Globals -> Atom -> Either String Value
val (Locals locals) (Globals globals) = \case
    AtomLit (Literal k) -> Right (PrimInt k)
    AtomVar v | Just addr <- M.lookup v locals  -> Right (Addr addr)
              | Just addr <- M.lookup v globals -> Right (Addr addr)
    _otherwise -> Left "Value of variable not found"

vals :: Locals -> Globals -> [Atom] -> Either String [Value]
vals locals globals = traverse (val locals globals)

data StgState = StgState
    { stgCode        :: Code
    , stgArgStack    :: Stack Value
    , stgReturnStack :: Stack ()
    , stgUpdateStack :: Stack ()
    , stgHeap        :: Heap
    , stgGlobals     :: Globals }

initialState :: Expr -> Globals -> StgState
initialState main' globals = StgState
    { stgCode        = Eval main' (Locals M.empty)
    , stgArgStack    = mempty
    , stgReturnStack = mempty
    , stgUpdateStack = mempty
    , stgHeap        = Heap M.empty
    , stgGlobals     = globals }

stgStep :: StgState -> Either String StgState
stgStep (StgState (Eval (AppF f@(Var _) xs) locals) argS retS updS heap globals) = do
    a <- case val locals globals (AtomVar f) of
        Right (Addr a')   -> Right a'
        Right (PrimInt k) -> Left "Got PrimInt, expected Addr"
        Left err          -> Left err
    xsVals <- vals locals globals xs
    let argS' = pushMany xsVals argS
    pure (StgState (Enter a) argS' retS updS heap globals)

stgStep (StgState (Enter a) argS retS updS heap globals)
    | Just (LambdaForm free update bound body) <- heapLookup a heap
    , Just (wsa, argS') <- popN (length bound) argS
  = Right (
    let locals = undefined
    in StgState (Eval body locals) argS' retS updS heap globals )

stgStep (StgState (Eval (Let rec binds expr) locals) argS retS updS heap globals)
  = Right (StgState (Eval expr locals' argS retS updS heap globals))
  where
    locals' = undefined

    localsRhs = case rec of
        NonRecursive -> locals
        Recursive    -> locals'
