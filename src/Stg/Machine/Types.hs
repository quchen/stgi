{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types used in the execution of the STG machine.
module Stg.Machine.Types where



import           Data.Map     (Map)

import           Stack
import           Stg.Language


data StgState = StgState
    { stgCode        :: Code
    , stgArgStack    :: Stack Value
    , stgReturnStack :: Stack (Alts, Locals)
    , stgUpdateStack :: Stack (Stack Value, Stack (Alts, Locals), MemAddr)
    , stgHeap        :: Heap
    , stgGlobals     :: Globals
    , stgTicks       :: Integer }

newtype MemAddr = MemAddr Int
    deriving (Eq, Ord, Show)

data Value = Addr MemAddr | PrimInt Int
    deriving (Eq, Ord, Show)

data Code = Eval Expr Locals
          | Enter MemAddr
          | ReturnCon Constr [Value]
          | ReturnInt Int
    deriving (Eq, Ord, Show)

newtype Globals = Globals (Map Var Value)
    deriving (Eq, Ord, Show, Monoid)

newtype Locals = Locals (Map Var Value)
    deriving (Eq, Ord, Show, Monoid)

data Closure = Closure LambdaForm [Value]
    deriving (Eq, Ord, Show)

newtype Heap = Heap (Map MemAddr Closure)
    deriving (Eq, Ord, Show, Monoid)
