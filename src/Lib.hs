{-# LANGUAGE LambdaCase #-}

module Stg where



import           Data.Map    (Map)
import qualified Data.Map    as M

import           Stack
import           StgLanguage



newtype Heap = Heap (Map MemAddr LambdaForm)

heapLookup :: MemAddr -> Heap -> Maybe LambdaForm
heapLookup addr (Heap heap) = M.lookup addr heap


newtype MemAddr = MemAddr Int deriving (Eq, Ord, Show)
data Value = Addr MemAddr | PrimInt Int
data Code = Eval Expr Locals
          | Enter MemAddr
          | ReturnCon Constr [Value]
          | ReturnInt Int
data Globals = Globals (Map Var MemAddr)
data Locals = Locals (Map Var MemAddr)

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
    let locals = Locals (M.fromList (zip
    in StgState (Eval body locals) argS' retS updS heap globals)
