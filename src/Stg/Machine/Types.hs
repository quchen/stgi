{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
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
    Binding(..),
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
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Monoid
import           GHC.Generics
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))
import           Text.Printf

import Stack
import Stg.Language
import Stg.Language.Prettyprint.Ansi
import Stg.Util



-- | The internal state of an STG.
data StgState = StgState
    { stgCode :: Code
        -- ^ Operation the STG should perform next

    , stgStack :: Stack StackFrame
        -- ^ The stack stores not-yet-used arguments (argument stack part),
        -- computations to return to once case evaluation has finished
        -- (return stack part), and instructions to update heap entries
        -- once computation of a certain value is done.

    , stgHeap :: Heap
        -- ^ The heap stores values allocated at the top level or in @let(rec)@
        --   expressions.

    , stgGlobals :: Globals
        -- ^ The environment consisting of the top-level definitions.

    , stgTicks :: Integer
        -- ^ A counter, used to generte fresh variable names from.

    , stgInfo :: Info
        -- ^ Information about the current state
    }
    deriving (Eq, Ord, Show, Generic)

-- | Package of colour definitions used in this module.
data StgStateColours = StgStateColours
    { headline :: Doc -> Doc
        -- ^ Style of headlines in the state overview, such as \"Heap" and
        --   "Frame i".
    , address :: Doc -> Doc
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
            (align . vsep) [ fill 7 (headline colour "Alts:")   <+> align (prettyAnsi alts)
                           , fill 7 (headline colour "Locals:") <+> align (prettyAnsi locals) ]
        UpdateFrame addr -> "(Update)" <+> prettyAnsi addr

-- | A memory address.
newtype MemAddr = MemAddr Int
    deriving (Eq, Ord, Show, Generic)

instance Pretty MemAddr where
    pretty (MemAddr addr) =  "0x" <> hexAddr addr
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

-- | Wrapper for nice 'Pretty' instances
data Mapping k v = Mapping k v

instance (Pretty k, Pretty v) => Pretty (Mapping k v) where
    pretty (Mapping k v) = pretty k <+> "->" <+> pretty v

instance (PrettyAnsi k, PrettyAnsi v) => PrettyAnsi (Mapping k v) where
    prettyAnsi (Mapping k v) = prettyAnsi k <+> "->" <+> prettyAnsi v

-- | Prettyprint a 'Map', @key -> value@.
prettyMap :: (Pretty k, Pretty v) => Map k v -> Doc
prettyMap m | M.null m = "(empty)"
prettyMap m = (align . vsep) [ pretty (Mapping k v) | (k,v) <- M.assocs m ]

-- | ANSI-prettyprint a 'Map', @key -> value@.
prettyAnsiMap :: (PrettyAnsi k, PrettyAnsi v) => Map k v -> Doc
prettyAnsiMap m | M.null m = "(empty)"
prettyAnsiMap m = (align . vsep) [ prettyAnsi (Mapping k v) | (k,v) <- M.assocs m ]

-- | The global environment consists of the mapping from top-level definitions
-- to their respective values.
newtype Globals = Globals (Map Var Value)
    deriving (Eq, Ord, Show, Monoid)

instance Pretty Globals where
    pretty (Globals globals) = prettyMap globals

instance PrettyAnsi Globals where
    prettyAnsi (Globals globals) = prettyAnsiMap globals

-- | A single association from 'Var'iable to 'Value'.
data Binding = Binding Var Value
    deriving (Eq, Ord, Show)

-- | The global environment consists if the mapping from local definitions
-- to their respective values.
newtype Locals = Locals (Map Var Value)
    deriving (Eq, Ord, Show, Monoid)

instance Pretty Locals where
    pretty (Locals locals) = prettyMap locals

instance PrettyAnsi Locals where
    prettyAnsi (Locals locals) = prettyAnsiMap locals

-- | User-facing information about the current state of the STG.
data Info = Info InfoShort [InfoDetail]
    deriving (Eq, Ord, Show, Generic)

instance Pretty Info where
    pretty = prettyInfo pretty prettyList

instance PrettyAnsi Info where
    prettyAnsi = prettyInfo prettyAnsi prettyAnsiList

prettyInfo :: (forall a. PrettyAnsi a => a -> Doc)
    -> (forall a. PrettyAnsi a => [a] -> Doc)
    -> Info
    -> Doc
prettyInfo ppr pprList = \case
    Info short []      -> ppr short
    Info short details -> vsep [ppr short, pprList details]

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
    pretty = prettyInfoShort pretty

instance PrettyAnsi InfoShort where
    prettyAnsi = prettyInfoShort prettyAnsi

prettyInfoShort :: (forall a. PrettyAnsi a => a -> Doc)
    -> InfoShort
    -> Doc
prettyInfoShort ppr = \case
    HaltedByPredicate -> "Halting predicate held"
    NoRulesApply      -> "No further rules apply"
    MaxStepsExceeded  -> "Maximum number of steps exceeded"
    StateError err    -> "Errorenous state: " <+> ppr err
    StateTransition t -> ppr t
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
        Eval_Case                     -> "Case evaluation"
        Eval_Case_Primop_Normal       -> "Case evaluation of primop: taking a shortcut, standard match"
        Eval_Case_Primop_DefaultBound -> "Case evaluation of primop: taking a shortcut, bound default match"
        Eval_FunctionApplication      -> "Function application"
        Eval_Let NonRecursive         -> "Let evaluation"
        Eval_Let Recursive            -> "Letrec evaluation"
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
    | UpdateClosureWithPrimitive
    | NonAlgPrimScrutinee
    | DivisionByZero
    deriving (Eq, Ord, Show, Generic)

instance Pretty StateError where
    pretty = \case
        VariablesNotInScope notInScope -> pretty notInScope <+> "not in scope"
        UpdatableClosureWithArgs -> "Closures with non-empty argument lists are never updatable"
        ReturnIntWithEmptyReturnStack -> "ReturnInt state with empty return stack"
        AlgReturnToPrimAlts -> "Algebraic constructor return to primitive alternatives"
        PrimReturnToAlgAlts -> "Primitive return to algebraic alternatives"
        InitialStateCreationFailed -> "Initial state creation failed"
        EnterBlackhole -> "Entering black hole"
        UpdateClosureWithPrimitive -> "Update closure with primitive value"
        NonAlgPrimScrutinee -> "Non-algebraic/primitive case scrutinee"
        DivisionByZero -> "Division by zero"

instance PrettyAnsi StateError where
    prettyAnsi = \case
        VariablesNotInScope notInScope
            -> prettyAnsi notInScope <+> "not in scope"
        x -> pretty x

data InfoDetail =
      Detail_FunctionApplication Var [Atom]
    | Detail_UnusedLocalVariables [Var] Locals
    | Detail_EnterNonUpdatable MemAddr [Value]
    | Detail_EvalLet [Var] [MemAddr]
    | Detail_EvalCase
    | Detail_ReturnConDefBound Var MemAddr
    | Detail_ReturnIntDefBound Var Integer
    | Detail_EnterUpdatable MemAddr
    | Detail_ConUpdate Constr MemAddr
    | Detail_PapUpdate MemAddr
    | Detail_ReturnIntCannotUpdate
    | Detail_StackNotEmpty
    | Detail_GarbageCollected [MemAddr]
    | Detail_EnterBlackHole MemAddr Integer
    | Detail_UpdateClosureWithPrimitive
    deriving (Eq, Ord, Show, Generic)

instance Pretty InfoDetail where
    pretty = prettyInfoDetail pretty
    prettyList = vsep . map pretty

instance PrettyAnsi InfoDetail where
    prettyAnsi = prettyInfoDetail prettyAnsi
    prettyAnsiList = vsep . map prettyAnsi

prettyInfoDetail :: (forall a. PrettyAnsi a => a -> Doc) -> InfoDetail -> Doc
prettyInfoDetail ppr items = bulletList (case items of
    Detail_FunctionApplication val [] ->
        ["Inspect value" <+> ppr val]
    Detail_FunctionApplication function args ->
        [ "Apply function"
          <+> ppr function
          <+> "to argument" <> pluralS args
          <+> commaSep (map ppr args) ]

    Detail_UnusedLocalVariables vars (Locals locals) ->
        let used = M.fromList [ (var, ()) | var <- vars ]
            unused = locals `M.difference` used
            pprDiscardedBind var val = [ppr var <+> "(" <> ppr val <> ")"]
        in if
            | M.null locals -> ["No local values were present, so none was discarded"]
            | M.null unused -> ["All locals were used, so none was discarded"]
            | otherwise ->
                ["Unused local variable" <> pluralS (M.toList unused) <+> "discarded:"
                 <+> commaSep (M.foldMapWithKey pprDiscardedBind unused)]

    Detail_EnterNonUpdatable addr args ->
        [ "Enter closure at" <+> ppr addr
        , if null args
            then ppr addr <+> "is a value, so no arguments are popped"
            else "Pop arguments" <+> commaSep (foldMap (\arg -> [ppr arg]) args) <+> "from the stack" ]

    Detail_EvalLet vars addrs ->
        [ hsep
            [ "Local environment extended by"
            , commaSep (foldMap (\var -> [ppr var]) vars) ]
        , hsep
            [ "Allocate new closure" <> pluralS vars <+> "at"
            , commaSep (zipWith (\var addr -> ppr addr <+> "(" <> ppr var <> ")") vars addrs)
            , "on the heap" ]]

    Detail_EvalCase ->
        ["Save alternatives and local environment as a stack frame"]

    Detail_ReturnConDefBound var addr ->
        [ "Allocate closure at" <+> ppr addr <+> "for the bound value"
        , "Extend local environment with" <+> ppr (Mapping var addr) ]

    Detail_ReturnIntDefBound var i ->
        [ "Extend local environment with" <+> ppr (Mapping var (PrimInt i)) ]

    Detail_EnterUpdatable addr ->
        [ "Push a new update frame with the entered address" <+> ppr addr
        , "Overwrite the heap object at" <+> ppr addr <+> "with a black hole" ]

    Detail_ConUpdate con addrU ->
        [ "Trying to return" <+> ppr con <> ", but there is no return frame on the top of the stack"
        , "Update closure at" <+> ppr addrU <+> "given by the update frame with returned constructor"  ]

    Detail_PapUpdate updAddr ->
        [ "Not enough arguments on the stack"
        , "Try to reveal more arguments by performing the update for" <+> ppr updAddr ]

    Detail_ReturnIntCannotUpdate ->
        ["No closure has primitive type, so we cannot update one with a primitive int"]

    Detail_StackNotEmpty ->
        [ "The stack is not empty; the program terminated unexpectedly."
        , "The lack of a better description is a bug in the STG evaluator."
        , "Please report this to the project maintainers!" ]

    Detail_GarbageCollected addrs ->  ["Removed address" <> pluralES addrs <> ":" <+> pprAddrs addrs]
      where
        pprAddrs = ppr . commaSep . foldMap (\addr -> [ppr addr])
        pluralES [_] = ""
        pluralES _ = "es"

    Detail_EnterBlackHole addr tick ->
        [ "Heap address" <+> ppr addr <+> "is a black hole, created in step" <+> ppr tick
        , "Entering a black hole means a thunk depends on its own evaluation"
        , "This is the functional equivalent of an infinite loop"
        , "GHC reports this condition as \"<<loop>>\"" ]

    Detail_UpdateClosureWithPrimitive ->
        [ "A closure never has primitive type, so it cannot be updated with a primitive value" ] )

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
        prettyFree vars = tupled' (zipWith (\k v -> pretty (Mapping k v)) vars freeVals)

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
        prettyFree vars = tupled' (zipWith (\k v -> prettyAnsi (Mapping k v)) vars freeVals)

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

prettyHeapObject :: (Closure -> Doc) -> (Doc -> Doc) -> HeapObject -> Doc
prettyHeapObject pprClosure style = \case
    HClosure closure@(Closure lambda _freeVals) ->
        style (classify lambda) <+> align (pprClosure closure)
    Blackhole tick -> style "BLACKHOLE" <+> "(from step " <> integer tick <> ")"
  where
    classify = \case
        LambdaForm _ _ [] AppC{} -> "CON"
        LambdaForm _ _ (_:_) _   -> "FUN"
        LambdaForm _ _ []    _   -> "THUNK"
        LambdaForm{} -> "" -- Fallthrough pattern to silence inexhaustive
                           -- warning by GHC 7.10.3. Probably a compiler
                           -- bug.

instance Pretty HeapObject where
    pretty = prettyHeapObject pretty id

instance PrettyAnsi HeapObject where
    prettyAnsi = prettyHeapObject prettyAnsi bold
