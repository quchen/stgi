{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Types used in the execution of the STG machine.
module Stg.Machine.Types (
    StgState(..),
    StgStateColours(..),
    StackFrame(..),
    MemAddr(..),
    Value(..),
    Code(..),
    Globals(..),
    Locals(..),
    Closure(..),
    Heap(..),
    HeapObject(..),

    -- * State information
    Info(..),
    InfoShort(..),
    InfoDetail(..),
    StateTransition(..),
    StateError(..),
    NotInScope(..),
) where



import           Data.Foldable
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified GHC.Exts                      as Exts
import           GHC.Generics
import           Text.PrettyPrint.ANSI.Leijen  hiding ((<>))
import           Text.Printf

import           Stack
import           Stg.Language
import           Stg.Language.Prettyprint.Ansi
import           Stg.Util



-- | The internal state of an STG.
data StgState = StgState
    { stgCode    :: Code
        -- ^ Operation the STG should perform next

    , stgStack   :: Stack StackFrame
        -- ^ The stack stores not-yet-used arguments (argument stack part),
        -- computations to return to once case evaluation has finished
        -- (return stack part), and instructions to update heap entries
        -- once computation of a certain value is done.

    , stgHeap    :: Heap
        -- ^ The heap stores values allocated at the top level or in @let(rec)@
        --   expressions.

    , stgGlobals :: Globals
        -- ^ The environment consisting of the top-level definitions.

    , stgTicks   :: Integer
        -- ^ A counter, used to generte fresh variable names from.

    , stgInfo    :: Info
        -- ^ Information about the current state
    }
    deriving (Eq, Ord, Show, Generic)

-- | Package of colour definitions used in this module.
data StgStateColours = StgStateColours
    { headline :: Doc -> Doc
        -- ^ Style of headlines in the state overview, such as \"Heap" and
        --   "Frame i".
    , address  :: Doc -> Doc
        -- ^ Style of memory addresses.
    }

-- | Colour definitions used in this module.
colour :: StgStateColours
colour = StgStateColours
    { headline = dullblue
    , address = dullcyan . underline
    }

instance Pretty StgState where
    pretty state = align (vsep
        [ "Code:" <+> pretty (stgCode state)
        , nest 4 (vsep [ "Stack", prettyStack (stgStack state) ])
        , nest 4 (vsep [ "Heap", pretty (stgHeap state)])
        , nest 4 (vsep [ "Globals", pretty (stgGlobals state)])
        , nest 4 ("Step:" <+> pretty (stgTicks state)) ])

instance PrettyAnsi StgState where
    prettyAnsi state = align (vsep
        [ headline colour "Code:" <+> prettyAnsi (stgCode state)
        , nest 4 (vsep [headline colour "Stack", prettyStackAnsi (stgStack state) ])
        , nest 4 (vsep [headline colour "Heap", prettyAnsi (stgHeap state)])
        , nest 4 (vsep [headline colour "Globals", prettyAnsi (stgGlobals state)])
        , nest 4 (headline colour "Step:" <+> pretty (stgTicks state)) ])

-- | Prettyprint a 'Stack'.
prettyStack :: Pretty a => Stack a -> Doc
prettyStack Empty = "(empty)"
prettyStack stack = (align . vsep) prettyFrames
  where
    prettyFrame frame i = "[" <+> int i <> "]" <+> align (pretty frame)
    prettyFrames = zipWith prettyFrame (toList stack) [1..]

-- | ANSI-prettyprint a 'Stack'.
prettyStackAnsi :: PrettyAnsi a => Stack a -> Doc
prettyStackAnsi Empty = "(empty)"
prettyStackAnsi stack = (align . vsep) prettyFrames
  where
    prettyFrame frame i = "[" <> int i <> "]" <+> align (prettyAnsi frame)
    prettyFrames = zipWith prettyFrame (toList stack) (reverse [1..length stack])

-- ^ A stack frame of the unified stack that includes arguments, returns, and
-- updates.
data StackFrame =
      ArgumentFrame Value
        -- ^ Argument frames store values on the argument stack, so that they
        -- can later be retrieved when the calling function can be applied to
        -- them.

    | ReturnFrame Alts Locals
        -- ^ Return frames are used when the scrutinee of a case expression is
        -- done being evaluated, and the branch to continue on has to be
        -- decided.

    | UpdateFrame MemAddr
        -- ^ When an updatable closure is entered, an update frame with its heap
        -- address is created. Once its computation finishes, its heap entry is
        -- updated with the computed value.

    deriving (Eq, Ord, Show, Generic)

instance Pretty StackFrame where
    pretty = \case
        ArgumentFrame val -> "(Argument)" <+> pretty val
        ReturnFrame alts locals -> "(Return)" <+>
            (align . vsep) [ fill 7 "Alts:"   <+> align (pretty alts)
                           , fill 7 "Locals:" <+> align (pretty locals) ]
        UpdateFrame addr -> "(Update)" <+> pretty addr

instance PrettyAnsi StackFrame where
    prettyAnsi = \case
        ArgumentFrame val -> "(Argument)" <+> prettyAnsi val
        ReturnFrame alts locals -> "(Return)" <+>
            (align . vsep) [ fill 7 "Alts:" <+> align (prettyAnsi alts)
                           , fill 7 "Locals:" <+> align (prettyAnsi locals) ]
        UpdateFrame addr -> "(Update)" <+> prettyAnsi addr

-- | A memory address.
newtype MemAddr = MemAddr Int
    deriving (Eq, Ord, Show, Generic)



instance Pretty MemAddr where
    pretty (MemAddr addr) =  "^" <> hexAddr addr
      where
        hexAddr = text . printf "%02x"

instance PrettyAnsi MemAddr where
    prettyAnsi (MemAddr addr) = address colour (hexAddr addr)
      where
        hexAddr = text . printf "%02x"

-- | A value of the STG machine.
data Value = Addr MemAddr | PrimInt Integer
    deriving (Eq, Ord, Show, Generic)

instance Pretty Value where
    pretty = \case
        Addr addr -> pretty addr
        PrimInt i -> pretty i <> "#"
    prettyList = tupled . map pretty

instance PrettyAnsi Value where
    prettyAnsi = \case
        Addr addr -> prettyAnsi addr
        PrimInt i -> prettyAnsi (Literal i)
    prettyAnsiList = tupled . map prettyAnsi

-- | The different code states the STG can be in.
data Code =
          -- | Evaluate an expression within a local environment
          Eval Expr Locals

          -- | Load the closure at a certain heap address
        | Enter MemAddr

          -- | Sub-computation terminated with algebraic constructor
        | ReturnCon Constr [Value]

          -- | Sub-computation terminated with a primitive integer
        | ReturnInt Integer
    deriving (Eq, Ord, Show, Generic)

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
            , headline colour "Locals:" <+> prettyAnsi locals ]
        Enter addr -> "Enter" <+> prettyAnsi addr
        ReturnCon constr args -> "ReturnCon" <+> prettyAnsi constr <+> prettyAnsiList args
        ReturnInt i -> "ReturnInt" <+> prettyAnsi (Literal i)

-- | Prettyprint a 'Map', @key -> value@.
prettyMap :: (Pretty k, Pretty v) => Map k v -> Doc
prettyMap m | M.null m = "(empty)"
prettyMap m = (align . vsep)
    [ pretty k <+> "->" <+> pretty v
    | (k,v) <- M.assocs m ]

-- | ANSI-prettyprint a 'Map', @key -> value@.
prettyAnsiMap :: (PrettyAnsi k, PrettyAnsi v) => Map k v -> Doc
prettyAnsiMap m | M.null m = "(empty)"
prettyAnsiMap m = (align . vsep)
    [ prettyAnsi k <+> "->" <+> prettyAnsi v
    | (k,v) <- M.assocs m ]

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

-- | User-facing information about the current state of the STG.
data Info = Info InfoShort InfoDetail
    deriving (Eq, Ord, Show, Generic)

instance Pretty Info where
    pretty (Info short []) = pretty short
    pretty (Info short details) = vsep [pretty short, pretty details]

instance PrettyAnsi Info

-- | Short machine status info. This field may be used programmatically, in
-- particular it tells the stepper whether the machine has halted.
data InfoShort =
      NoRulesApply
      -- ^ There is no valid state transition to continue with.

    | MaxStepsExceeded
      -- ^ The machine did not halt within a number of steps. Used by
      -- 'Stg.Machine.evalUntil'.

    | HaltedByPredicate
      -- ^ The machine halted because a user-specified halting predicate
      -- held.

    | StateError StateError
      -- ^ The machine halted in a state that is known to be invalid, there is
      -- no valid state transition to continue with.
      --
      -- An example of this would be a 'ReturnCon' state with an empty
      -- return stack.

    | StateTransition StateTransition
      -- ^ Description of the state transition that lead to the current state.

    | StateInitial
      -- ^ Used to mark the initial state of the machine.

    | GarbageCollection
      -- ^ A garbage collection step, in which no ordinary evaluation is done.
    deriving (Eq, Ord, Show, Generic)

instance Pretty InfoShort where
    pretty = \case
        HaltedByPredicate -> "Halting predicate held"
        NoRulesApply      -> "No further rules apply"
        MaxStepsExceeded  -> "Maximum number of steps exceeded"
        StateError err    -> "Errorenous state: " <+> pretty err
        StateTransition t -> "State transition:" <+> pretty t
        StateInitial      -> "Initial state"
        GarbageCollection -> "Garbage collection"

data StateTransition =
      Enter_NonUpdatableClosure
    | Enter_PartiallyAppliedUpdate
    | Enter_UpdatableClosure
    | Eval_AppC
    | Eval_AppP
    | Eval_Case
    | Eval_Case_Primop_Normal
    | Eval_Case_Primop_DefaultBound
    | Eval_FunctionApplication
    | Eval_Let Rec
    | Eval_Lit
    | Eval_LitApp
    | ReturnCon_DefBound
    | ReturnCon_DefUnbound
    | ReturnCon_Match
    | ReturnCon_Update
    | ReturnInt_DefBound
    | ReturnInt_DefUnbound
    | ReturnInt_Match
    deriving (Eq, Ord, Show, Generic)

instance Pretty StateTransition where
    pretty = \case
        Enter_NonUpdatableClosure     -> "Enter non-updatable closure"
        Enter_PartiallyAppliedUpdate  -> "Enter partially applied closure"
        Enter_UpdatableClosure        -> "Enter updatable closure"
        Eval_AppC                     -> "Constructor application"
        Eval_AppP                     -> "Primitive function application"
        Eval_Case                     -> "case evaluation"
        Eval_Case_Primop_Normal       -> "case evaluation of primop: taking a shortcut, standard match"
        Eval_Case_Primop_DefaultBound -> "case evaluation of primop: taking a shortcut, bound default match"
        Eval_FunctionApplication      -> "Function application"
        Eval_Let NonRecursive         -> "let evaluation"
        Eval_Let Recursive            -> "letrec evaluation"
        Eval_Lit                      -> "Literal evaluation"
        Eval_LitApp                   -> "Literal application"
        ReturnCon_DefBound            -> "Algebraic constructor return, bound default match"
        ReturnCon_DefUnbound          -> "Algebraic constructor return, unbound default match"
        ReturnCon_Match               -> "Algebraic constructor return, standard match"
        ReturnCon_Update              -> "Update by constructor return"
        ReturnInt_DefBound            -> "Primitive constructor return, bound default match"
        ReturnInt_DefUnbound          -> "Primitive constructor return, unbound default match"
        ReturnInt_Match               -> "Primitive constructor return, standard match found"

instance PrettyAnsi StateTransition

-- | Type safety wrapper.
newtype NotInScope = NotInScope [Var]
    deriving (Eq, Ord, Show, Monoid)

instance Pretty NotInScope where
    pretty (NotInScope vars) = commaSep (map pretty vars)

instance PrettyAnsi NotInScope where
    prettyAnsi (NotInScope vars) = commaSep (map prettyAnsi vars)

data StateError =
      VariablesNotInScope NotInScope
    | UpdatableClosureWithArgs
    | ReturnIntWithEmptyReturnStack
    | AlgReturnToPrimAlts
    | PrimReturnToAlgAlts
    | InitialStateCreationFailed
    | EnterBlackhole
    deriving (Eq, Ord, Show, Generic)

instance Pretty StateError where
    pretty = \case
        VariablesNotInScope notInScope
            -> pretty notInScope <+> "not in scope"
        UpdatableClosureWithArgs
            -> "Closures with non-empty argument lists are never updatable"
        ReturnIntWithEmptyReturnStack
            -> "ReturnInt state with empty return stack"
        AlgReturnToPrimAlts
            -> "Algebraic constructor return to primitive alternatives"
        PrimReturnToAlgAlts
            -> "Primitive return to algebraic alternatives"
        InitialStateCreationFailed
            -> "Initial state creation failed"
        EnterBlackhole
            -> "Entering black hole"

instance PrettyAnsi StateError where
    prettyAnsi = \case
        VariablesNotInScope notInScope
            -> prettyAnsi notInScope <+> "not in scope"
        x -> pretty x

-- | Detailed information that may be useful to the user. Not used
-- programmatically.
newtype InfoDetail = InfoDetail [Text]
    deriving (Eq, Ord, Show, Generic, Monoid)

instance Exts.IsList InfoDetail where
    type Item InfoDetail = Text
    toList = Exts.coerce
    fromList = Exts.coerce

instance Pretty InfoDetail where
    pretty (InfoDetail entries) = case entries of
        [] -> mempty
        es -> let bulletPrefixes = map ("  -" <+>)
                  prettyWords = hsep . map (text . T.unpack) . T.words
              in (align . vsep . bulletPrefixes . map prettyWords) es



-- | A closure is a lambda form, together with the values of its free variables.
data Closure = Closure LambdaForm [Value]
    deriving (Eq, Ord, Show, Generic)

instance Pretty Closure where
    pretty (Closure lambdaForm []) = pretty lambdaForm
    pretty (Closure lambda freeVals) =
        prettyLambda id
                     prettyFree
                     pretty
                     prettyList
                     pretty
                     lambda
      where
        prettyFree vars = tupled' (zipWith
            (\var val -> pretty var <+> "->" <+> pretty val)
            vars
            freeVals )

instance PrettyAnsi Closure where
    prettyAnsi (Closure lambdaForm []) = prettyAnsi lambdaForm
    prettyAnsi (Closure lambda freeVals) =
        prettyLambda id -- FIXME: this should be lambdaHead to match the Ansi colour version
                     prettyFree
                     prettyAnsi
                     prettyAnsiList
                     prettyAnsi
                     lambda
      where
        prettyFree vars = tupled' (zipWith
            (\var val -> prettyAnsi var <+> "->" <+> prettyAnsi val)
            vars
            freeVals )

-- | The heap stores closures addressed by memory location.
newtype Heap = Heap (Map MemAddr HeapObject)
    deriving (Eq, Ord, Show, Monoid)

instance Pretty Heap where
    pretty (Heap heap) = prettyMap heap

instance PrettyAnsi Heap where
    prettyAnsi (Heap heap) = prettyAnsiMap heap

data HeapObject =
      HClosure Closure
    | Blackhole Integer
        -- ^ When an updatable closure is entered, it is overwritten by a
        -- black hole. This has two main benefits:
        --
        -- 1. Memory mentioned only in the closure is now ready to be collected
        -- 2. Entering a black hole means a thunk depends on itself, allowing
        --    the interpreter to catch some non-terminating computations with
        --    a useful error
        --
        -- To make the black hole a bit more transparent, it is tagged with
        -- the STG tick in which it was introduced. This tag is used only for
        -- display purposes.
    deriving (Eq, Ord, Show)

instance Pretty HeapObject where
    pretty = \case
        HClosure closure -> "FUN" <+> pretty closure
        Blackhole tick -> "BLACKHOLE (created in step " <> integer tick <> ")"

instance PrettyAnsi HeapObject where
    prettyAnsi = \case
        HClosure closure -> "FUN" <+> prettyAnsi closure
        Blackhole tick -> "BLACKHOLE (created in step " <> integer tick <> ")"
