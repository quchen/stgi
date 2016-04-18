{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

-- | Types used in the execution of the STG machine.
module Stg.Machine.Types where



import           Data.Foldable
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Numeric
import           Text.PrettyPrint.ANSI.Leijen  hiding ((<>))

import           Stack
import           Stg.Language
import           Stg.Language.Prettyprint.Ansi



-- | The internal state of an STG.
data StgState = StgState
    { stgCode        :: Code
        -- ^ Operation the STG should perform next

    , stgArgStack    :: Stack ArgumentFrame
        -- ^ Argument stack, storing values given to functions and constructors

    , stgReturnStack :: Stack ReturnFrame
        -- ^ Return stack, storing the local environment to restore and
        --   possible alternatives to be taken once a computation reaches
        --   a certain point.

    , stgUpdateStack :: Stack UpdateFrame
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

    , stgInfo        :: Info
        -- ^ Information about the current state
    }
    deriving (Show)

data StgStateColours = StgStateColours
    { headline :: Doc -> Doc
    , number   :: Doc -> Doc
    }

colours :: StgStateColours
colours = StgStateColours
    { headline = dullblue
    , number = dullgreen
    }

instance Pretty StgState where
    pretty state = nest 4 (vsep ["STG state", align (vsep
        [ "Code:" <+> pretty (stgCode state)
        , nest 4 (vsep
            [ "Stacks"
            , align (vsep
                [ "Argument:" <+> prettyStack (align . vsep) (stgArgStack state)
                , "Return:  " <+> prettyStack (align . vsep) (stgReturnStack state)
                , "Update:  " <+> prettyStack (align . vsep) (stgUpdateStack state) ])])
        , nest 4 (vsep [ "Heap", pretty (stgHeap state)])
        , nest 4 (vsep [ "Globals", pretty (stgGlobals state)])
        , nest 4 ("Step:" <+> pretty (stgTicks state)) ])])

instance PrettyAnsi StgState where
    prettyAnsi state = nest 4 (vsep ["STG state", align (vsep
        [ headline colours "Code:" <+> prettyAnsi (stgCode state)
        , nest 4 (vsep
            [headline colours "Stacks"
            , align (vsep
                [ "Argument:" <+> prettyStackAnsi (align . vsep) (stgArgStack state)
                , "Return:  " <+> prettyStackAnsi (align . vsep) (stgReturnStack state)
                , "Update:  " <+> prettyStackAnsi (align . vsep) (stgUpdateStack state) ])])
        , nest 4 (vsep [headline colours "Heap", prettyAnsi (stgHeap state)])
        , nest 4 (vsep [headline colours "Globals", prettyAnsi (stgGlobals state)])
        , nest 4 (headline colours "Step:" <+> pretty (stgTicks state)) ])])

prettyStack :: Pretty a => ([Doc] -> Doc) -> Stack a -> Doc
prettyStack _ Empty = "(empty)"
prettyStack separator s = separator [pretty x | x <- toList s]

prettyStackAnsi :: PrettyAnsi a => ([Doc] -> Doc) -> Stack a -> Doc
prettyStackAnsi _ Empty = "(empty)"
prettyStackAnsi separator s = separator [prettyAnsi x | x <- toList s]

-- | Argument frames store values on the argument stack, so that they can
-- later be retrieved when the calling function can be applied to them.
newtype ArgumentFrame = ArgumentFrame Value
    deriving (Eq, Ord, Show)

instance Pretty ArgumentFrame where
    pretty (ArgumentFrame val) = pretty val

instance PrettyAnsi ArgumentFrame where
    prettyAnsi (ArgumentFrame val) = prettyAnsi val

-- | Return frames are used when the scrutinee of a case expression is done
-- being evaluated, and the branch to continue on has to be decided.
data ReturnFrame = ReturnFrame Alts Locals
    deriving (Eq, Ord, Show)

instance Pretty ReturnFrame where
    pretty (ReturnFrame alts locals) =
        hsep [ pretty alts, "Locals:" <+> pretty locals ]

instance PrettyAnsi ReturnFrame where
    prettyAnsi (ReturnFrame alts locals) =
        hsep [ prettyAnsi alts, "Locals:" <+> prettyAnsi locals ]

-- | Update frames store information about the machine's state before an
-- updateable closure was entered, so that they can help update it once it is
-- evaluated.
data UpdateFrame = UpdateFrame (Stack ArgumentFrame) (Stack ReturnFrame) MemAddr
    deriving (Eq, Ord, Show)

instance Pretty UpdateFrame where
    pretty (UpdateFrame upd ret addr) =
        hsep [ prettyStack hsep upd
             , prettyStack hsep ret
             , pretty addr ]

instance PrettyAnsi UpdateFrame where
    prettyAnsi (UpdateFrame upd ret addr) =
        hsep [ prettyStackAnsi hsep upd
             , prettyStackAnsi hsep ret
             , prettyAnsi addr ]

-- | A memory address.
newtype MemAddr = MemAddr Int
    deriving (Eq, Ord, Show)

instance Pretty MemAddr where
    pretty (MemAddr addr) = (text . ("0x" <>) . ($ "") . showHex) addr

instance PrettyAnsi MemAddr where
    prettyAnsi (MemAddr addr) = number colours
        ((text . ("0x" <>) . ($ "") . showHex) addr)

-- | A value of the STG machine.
data Value = Addr MemAddr | PrimInt Integer
    deriving (Eq, Ord, Show)

instance Pretty Value where
    pretty = \case
        Addr addr -> pretty addr
        PrimInt i -> pretty i
    prettyList = parens . hsep . punctuate "," . map pretty

instance PrettyAnsi Value where
    prettyAnsi = \case
        Addr addr -> prettyAnsi addr
        PrimInt i -> prettyAnsi i
    prettyAnsiList = parens . hsep . punctuate "," . map prettyAnsi

-- | The different code states the STG can be in.
data Code = Eval Expr Locals
          | Enter MemAddr
          | ReturnCon Constr [Value]
          | ReturnInt Integer
    deriving (Eq, Ord, Show)

instance Pretty Code where
    pretty = \case
        Eval expr locals -> (align . vsep)
            [ "Eval" <+> pretty expr
            , "Locals:" <+> pretty locals ]
        Enter addr -> "Enter" <+> pretty addr
        ReturnCon constr args -> "ReturnCon" <+> pretty constr <+> prettyList args
        ReturnInt i -> "ReturnInt" <+> pretty i

instance PrettyAnsi Code where
    prettyAnsi = \case
        Eval expr locals -> (align . vsep)
            [ "Eval" <+> prettyAnsi expr
            , "Locals:" <+> prettyAnsi locals ]
        Enter addr -> "Enter" <+> prettyAnsi addr
        ReturnCon constr args -> "ReturnCon" <+> prettyAnsi constr <+> prettyAnsiList args
        ReturnInt i -> "ReturnInt" <+> prettyAnsi i

prettyMap :: (Pretty k, Pretty v) => Map k v -> Doc
prettyMap m = (align . vsep)
    [ pretty k <+> "->" <+> pretty v
    | (k,v) <- M.toList m ]

prettyAnsiMap :: (PrettyAnsi k, PrettyAnsi v) => Map k v -> Doc
prettyAnsiMap m = (align . vsep)
    [ prettyAnsi k <+> "->" <+> prettyAnsi v
    | (k,v) <- M.toList m ]

-- | The global environment consists of the mapping from top-level definitions
-- to their respective values.
newtype Globals = Globals (Map Var Value)
    deriving (Eq, Ord, Show, Monoid)

instance Pretty Globals where
    pretty (Globals globals) = prettyMap globals

instance PrettyAnsi Globals where
    prettyAnsi (Globals globals) = prettyAnsiMap globals


-- | The global environment consists if the mapping from local definitions
-- to their respective values.
newtype Locals = Locals (Map Var Value)
    deriving (Eq, Ord, Show, Monoid)

instance Pretty Locals where
    pretty (Locals locals) = prettyMap locals

instance PrettyAnsi Locals where
    prettyAnsi (Locals locals) = prettyAnsiMap locals

data Info =
      NoRulesApply
      -- ^ There is no valid state transition to continue with.

    | MaxStepsExceeded
      -- ^ The machine did not halt within a number of steps. Used by
      -- 'Stg.Machine.evalUntil'.

    | HaltedByPredicate
      -- ^ The machine halted because a user-specified halting predicate
      -- held.

    | StateError Text
      -- ^ The machine halted in a state that is known to be invalid, there is
      -- no valid state transition to continue with.
      --
      -- An example of this would be a 'ReturnCon' state with an empty
      -- return stack.

    | StateTransiton Text
      -- ^ Description of the state transition that lead to the current state.
    deriving (Eq, Ord, Show)

instance Pretty Info where
    pretty HaltedByPredicate  = "Halting predicate held"
    pretty NoRulesApply       = "No further rules apply"
    pretty MaxStepsExceeded   = "Maximum number of steps exceeded"
    pretty (StateError x)     = "Errorenous state: " <+> pretty (T.unpack x)
    pretty (StateTransiton x) = "State transition:" <+> pretty (T.unpack x)

instance PrettyAnsi Info

-- | A closure is a lambda form, together with the values of its free variables.
data Closure = Closure LambdaForm [Value]
    deriving (Eq, Ord, Show)

instance Pretty Closure where
    pretty (Closure lambdaForm []) = pretty lambdaForm
    pretty (Closure lambdaForm free) = (align . vsep)
        [ pretty lambdaForm
        , "where" <+> prettyList free ]

instance PrettyAnsi Closure where
    prettyAnsi (Closure lambdaForm []) = prettyAnsi lambdaForm
    prettyAnsi (Closure lambdaForm free) = (align . vsep)
        [ prettyAnsi lambdaForm
        , "where" <+> prettyAnsiList free ]

-- | The heap stores closures addressed by memory location.
newtype Heap = Heap (Map MemAddr Closure)
    deriving (Eq, Ord, Show, Monoid)

instance Pretty Heap where
    pretty (Heap heap) = prettyMap heap

instance PrettyAnsi Heap where
    prettyAnsi (Heap heap) = prettyAnsiMap heap
