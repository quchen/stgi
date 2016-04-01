{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stg.Machine.Types where



import           Data.Map     (Map)

import           Stg.Language



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
