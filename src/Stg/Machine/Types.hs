{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stg.Machine.Types where



import           Data.Map     (Map)

import           Stg.Language



newtype MemAddr = MemAddr Int
    deriving (Eq, Ord, Show)

data Value = Addr MemAddr | PrimInt Int

data Code = Eval Expr Locals
          | Enter MemAddr
          | ReturnCon Constr [Value]
          | ReturnInt Int

newtype Globals = Globals (Map Var Value)
    deriving (Monoid)

newtype Locals = Locals (Map Var Value)
    deriving (Monoid)

data Closure = Closure LambdaForm [Value]

newtype Heap = Heap (Map MemAddr Closure)
