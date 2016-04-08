{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types used in the execution of the STG machine.
module Stg.Machine.Types where



import           Data.Map     (Map)

import           Stack
import           Stg.Language



-- | The internal state of an STG.
data StgState = StgState
    { stgCode        :: Code
        -- ^ Operation the STG should perform next

    , stgArgStack    :: Stack Value
        -- ^ Argument stack, storing values given to functions and constructors

    , stgReturnStack :: Stack (Alts, Locals)
        -- ^ Return stack, storing the local environment to restore and
        --   possible alternatives to be taken once a computation reaches
        --   a certain point.

    , stgUpdateStack :: Stack (Stack Value, Stack (Alts, Locals), MemAddr)
        -- ^ Update stack, used to store the environment an updateable closure
        --   was called in, so that it can be updated in memory when it has
        --   been reduced.

    , stgHeap        :: Heap
        -- ^ The heap stores values allocated at the top level or in @let(rec)@
        --   expressions.

    , stgGlobals     :: Globals
        -- ^ The environment consisting of the top-level definitions.

    , stgTicks       :: Integer
        -- ^ A counter, used to generte fresh variable names from.
    }

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
