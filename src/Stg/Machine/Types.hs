{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types used in the execution of the STG machine.
module Stg.Machine.Types where



import           Data.Map     (Map)

import           Stack
import           Stg.Language



-- | The internal state of an STG.
data StgState = StgState
    { stgCode        :: Code
    , stgArgStack    :: Stack Value
    , stgReturnStack :: Stack (Alts, Locals)
    , stgUpdateStack :: Stack (Stack Value, Stack (Alts, Locals), MemAddr)
    , stgHeap        :: Heap
    , stgGlobals     :: Globals
    , stgTicks       :: Integer }

-- | A memory address.
newtype MemAddr = MemAddr Int
    deriving (Eq, Ord, Show)

-- | A value of the STG machine.
data Value = Addr MemAddr | PrimInt Int
    deriving (Eq, Ord, Show)

-- | The different code states the STG can be in.
data Code = Eval Expr Locals
          | Enter MemAddr
          | ReturnCon Constr [Value]
          | ReturnInt Int
    deriving (Eq, Ord, Show)

-- | The global environment consists of the mapping from top-level definitions
-- to their respective values.
newtype Globals = Globals (Map Var Value)
    deriving (Eq, Ord, Show, Monoid)

-- | The global environment consists if the mapping from local definitions
-- to their respective values.
newtype Locals = Locals (Map Var Value)
    deriving (Eq, Ord, Show, Monoid)

-- | A closure is a lambda form, together with the values of its free variables.
data Closure = Closure LambdaForm [Value]
    deriving (Eq, Ord, Show)

-- | The heap stores closures addressed by memory location.
newtype Heap = Heap (Map MemAddr Closure)
    deriving (Eq, Ord, Show, Monoid)
