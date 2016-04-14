{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Types used in the execution of the STG machine.
module Stg.Machine.Types where



import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Numeric
import           Text.PrettyPrint.ANSI.Leijen

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

instance Pretty StgState where
    pretty (StgState
        { stgCode        = code
        , stgArgStack    = argStack
        , stgReturnStack = returnStack
        , stgUpdateStack = updateStack
        , stgHeap        = heap
        , stgGlobals     = globals
        , stgTicks       = ticks })
      = nest 4 (vsep ["STG state", align (vsep
            [ "Code:"           <+> prettyCode
            , "Argument stack:" <+> prettyArgStack
            , "Return stack:"   <+> prettyReturnStack
            , "Update stack:"   <+> prettyUpdateStack
            , "Heap:"           <+> prettyHeap
            , "Globals: "       <+> prettyGlobals
            , "Ticks:"          <+> prettyTicks ])])
      where
        prettyCode        = pretty code
        prettyArgStack    = pretty argStack
        prettyReturnStack = pretty returnStack
        prettyUpdateStack = pretty updateStack
        prettyHeap        = pretty heap
        prettyGlobals     = pretty globals
        prettyTicks       = pretty ticks



-- | A memory address.
newtype MemAddr = MemAddr Int
    deriving (Eq, Ord, Show)

instance Pretty MemAddr where
    pretty (MemAddr addr) = (text . ($ "") . showHex) addr

-- | A value of the STG machine.
data Value = Addr MemAddr | PrimInt Int
    deriving (Eq, Ord, Show)

instance Pretty Value where
    pretty = \case
        Addr addr -> pretty addr
        PrimInt i -> pretty i

-- | The different code states the STG can be in.
data Code = Eval Expr Locals
          | Enter MemAddr
          | ReturnCon Constr [Value]
          | ReturnInt Int
    deriving (Eq, Ord, Show)

instance Pretty Code where
    pretty = \case
        Eval expr locals -> "Eval" <+> pretty expr <+> pretty locals
        Enter addr -> "Enter" <+> pretty addr
        ReturnCon constr args -> "ReturnCon" <+> pretty constr <+> pretty args
        ReturnInt i -> "ReturnInt" <+> pretty i

prettyMap :: (Pretty k, Pretty v) => Map k v -> Doc
prettyMap m = (align . vsep)
    [ pretty k <+> "->" <+> pretty v
    | (k,v) <- M.toList m ]

-- | The global environment consists of the mapping from top-level definitions
-- to their respective values.
newtype Globals = Globals (Map Var Value)
    deriving (Eq, Ord, Show, Monoid)

instance Pretty Globals where
    pretty (Globals globals) = prettyMap globals

-- | The global environment consists if the mapping from local definitions
-- to their respective values.
newtype Locals = Locals (Map Var Value)
    deriving (Eq, Ord, Show, Monoid)

instance Pretty Locals where
    pretty (Locals locals) = prettyMap locals

-- | A closure is a lambda form, together with the values of its free variables.
data Closure = Closure LambdaForm [Value]
    deriving (Eq, Ord, Show)

instance Pretty Closure where
    pretty (Closure lambdaForm free) = braces (pretty lambdaForm) <+> pretty free

-- | The heap stores closures addressed by memory location.
newtype Heap = Heap (Map MemAddr Closure)
    deriving (Eq, Ord, Show, Monoid)

instance Pretty Heap where
    pretty (Heap heap) = "Heap:" <+> prettyMap heap
