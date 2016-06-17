{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Types used in the execution of the STG machine.
module Stg.Machine.Types (
    StgState(..),
    StgStateStyle(..),
    StackFrame(..),
    MemAddr(..),
    Value(..),
    Code(..),
    Mapping(..),
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



import           Control.DeepSeq
import           Data.Foldable
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Monoid
import           Data.Set                     (Set)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           GHC.Generics
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))
import           Text.Printf

import Data.Stack
import Stg.Language
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

    , stgSteps :: !Integer
        -- ^ A counter, used to generte fresh variable names from.

    , stgInfo :: Info
        -- ^ Information about the current state
    }
    deriving (Eq, Ord, Show, Generic)

-- | Package of style definitions used in this module.
data StgStateStyle = StgStateStyle
    { headline :: Doc -> Doc
        -- ^ Style of headlines in the state overview, such as \"Heap" and
        --   "Frame i".
    , address :: Doc -> Doc
        -- ^ Style of memory addresses, including @0x@ prefix.
    , addressCore :: Doc -> Doc
        -- ^ Style of memory addresses; applied only to the actual address
        --   number, such as @ff@ in @0xff@.

    , closureType :: Doc -> Doc
        -- ^ Style of the type of a closure, such as BLACKHOLE or FUN.

    , stackFrameType :: Doc -> Doc
        -- ^ Style of the stack frame annotation, such as UPD or ARG.
    }

-- | Colour definitions used in this module.
style :: StgStateStyle
style = StgStateStyle
    { headline       = dullblue
    , address        = dullcyan
    , addressCore    = underline
    , closureType    = bold
    , stackFrameType = bold
    }

-- Local re-definition to avoid cyclic import with the Heap module
heapSize :: Heap -> Int
heapSize (Heap h) = length h

instance Pretty StgState where
    pretty state = align (vsep
        [ headline style "Code:" <+> pretty (stgCode state)
        , nest 4 (vsep [headline style "Stack", prettyStack (stgStack state) ])
        , nest 4 (vsep [headline style "Heap" <> " (" <> pretty (heapSize (stgHeap state)) <+> "entries)"
                       , pretty (stgHeap state) ])
        , nest 4 (vsep [headline style "Globals", pretty (stgGlobals state)])
        , nest 4 (headline style "Step:" <+> pretty (stgSteps state)) ])

-- | Prettyprint a 'Stack'.
prettyStack :: Pretty a => Stack a -> Doc
prettyStack Empty = "(empty)"
prettyStack stack = (align . vsep) prettyFrames
  where
    prettyFrame frame i = hsep
        [ headline style (int i <> ".")
        , align (pretty frame) ]
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
        ArgumentFrame val -> stackFrameType style "Arg" <+> pretty val
        ReturnFrame alts locals -> stackFrameType style "Ret" <+>
            (align . vsep) [ fill 7 (headline style "Alts:")   <+> align (pretty alts)
                           , fill 7 (headline style "Locals:") <+> align (pretty locals) ]
        UpdateFrame addr -> stackFrameType style "Upd" <+> pretty addr

-- | A memory address.
newtype MemAddr = MemAddr Int
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Pretty MemAddr where
    pretty (MemAddr addr) = address style ("0x" <> addressCore style (hexAddr addr))
      where
        hexAddr = text . printf "%02x"

-- | A value of the STG machine.
data Value = Addr MemAddr | PrimInt Integer
    deriving (Eq, Ord, Show, Generic)

instance Pretty Value where
    pretty = \case
        Addr addr -> pretty addr
        PrimInt i -> pretty (Literal i)
    prettyList = tupled . map pretty

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
            , headline style "Locals:" <+> pretty locals ]
        Enter addr -> "Enter" <+> pretty addr
        ReturnCon constr args -> "ReturnCon" <+> pretty constr <+> prettyList args
        ReturnInt i -> "ReturnInt" <+> pretty (Literal i)

-- | A single key -> value association.
data Mapping k v = Mapping k v
    deriving (Eq, Ord, Show, Generic)

instance (Pretty k, Pretty v) => Pretty (Mapping k v) where
    pretty (Mapping k v) = pretty k <+> "->" <+> pretty v

-- | Prettyprint a 'Map', @key -> value@.
prettyMap :: (Pretty k, Pretty v) => Map k v -> Doc
prettyMap m | M.null m = "(empty)"
prettyMap m = (align . vsep) [ pretty (Mapping k v) | (k,v) <- M.assocs m ]

-- | The global environment consists of the mapping from top-level definitions
-- to their respective values.
newtype Globals = Globals (Map Var Value)
    deriving (Eq, Ord, Show, Monoid, Generic)

instance Pretty Globals where
    pretty (Globals globals) = prettyMap globals

-- | The global environment consists if the mapping from local definitions
-- to their respective values.
newtype Locals = Locals (Map Var Value)
    deriving (Eq, Ord, Show, Monoid, Generic)

instance Pretty Locals where
    pretty (Locals locals) = prettyMap locals

-- | User-facing information about the current state of the STG.
data Info = Info InfoShort [InfoDetail]
    deriving (Eq, Ord, Show, Generic)

instance Pretty Info where
    pretty = \case
        Info short []      -> pretty short
        Info short details -> vsep [pretty short, prettyList details]

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
        StateError err    -> "Errorenous state:" <+> pretty err
        StateTransition t -> pretty t
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

-- | Type safety wrapper.
newtype NotInScope = NotInScope [Var]
    deriving (Eq, Ord, Show, Generic, Monoid)

instance Pretty NotInScope where
    pretty (NotInScope vars) = commaSep (map pretty vars)

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
    | BadConArity Int Int
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
        BadConArity retArity altArity -> "Return" <+> pprArity retArity
                                     <+> "constructor to" <+> pprArity altArity
                                     <+> "alternative"

pprArity :: Int -> Doc
pprArity = \case
    0 -> "nullary"
    1 -> "unary"
    2 -> "binary"
    3 -> "ternary"
    n -> int n <> "-ary"

data InfoDetail =
      Detail_FunctionApplication Var [Atom]
    | Detail_UnusedLocalVariables [Var] Locals
    | Detail_EnterNonUpdatable MemAddr [Mapping Var Value]
    | Detail_EvalLet [Var] [MemAddr]
    | Detail_EvalCase
    | Detail_ReturnCon_Match Constr [Var]
    | Detail_ReturnConDefBound Var MemAddr
    | Detail_ReturnIntDefBound Var Integer
    | Detail_EnterUpdatable MemAddr
    | Detail_ConUpdate Constr MemAddr
    | Detail_PapUpdate MemAddr
    | Detail_ReturnIntCannotUpdate
    | Detail_StackNotEmpty
    | Detail_GarbageCollected Text (Set MemAddr) (Map MemAddr MemAddr)
    | Detail_EnterBlackHole MemAddr Integer
    | Detail_UpdateClosureWithPrimitive
    | Detail_BadConArity
    deriving (Eq, Ord, Show, Generic)

instance Pretty InfoDetail where
    prettyList = vsep . map pretty
    pretty items = bulletList (case items of
        Detail_FunctionApplication val [] ->
            ["Inspect value" <+> pretty val]
        Detail_FunctionApplication function args ->
            [ "Apply function"
              <+> pretty function
              <+> "to argument" <> pluralS args
              <+> commaSep (map pretty args) ]

        Detail_UnusedLocalVariables usedVars (Locals locals) ->
            let used = M.fromList [ (var, ()) | var <- usedVars ]
                unused = locals `M.difference` used
                pprDiscardedBind var val = [pretty var <+> "(" <> pretty val <> ")"]
            in ["Unused local variable" <> pluralS (M.toList unused) <+> "discarded:"
                <+> case unused of
                    [] -> "(none)"
                    _  -> commaSep (M.foldMapWithKey pprDiscardedBind unused) ]

        Detail_EnterNonUpdatable addr args ->
            [ "Enter closure at" <+> pretty addr
            , if null args
                then pretty addr <+> "does not take any arguments, so no frames are popped"
                else hang 4 (vsep
                        [ "Extend local environment with mappings from bound values to argument frame addresses:"
                        , commaSep (foldMap (\arg -> [pretty arg]) args) ])]

        Detail_EvalLet vars addrs ->
            [ hsep
                [ "Local environment extended by"
                , commaSep (foldMap (\var -> [pretty var]) vars) ]
            , hsep
                [ "Allocate new closure" <> pluralS vars <+> "at"
                , commaSep (zipWith (\var addr -> pretty addr <+> "(" <> pretty var <> ")") vars addrs)
                , "on the heap" ]]

        Detail_EvalCase ->
            ["Save alternatives and local environment as a stack frame"]

        Detail_ReturnCon_Match con args ->
            ["Pattern" <+> pretty (AppC con (map AtomVar args)) <+> "matches, follow its branch"]

        Detail_ReturnConDefBound var addr ->
            [ "Allocate closure at" <+> pretty addr <+> "for the bound value"
            , "Extend local environment with" <+> pretty (Mapping var addr) ]

        Detail_ReturnIntDefBound var i ->
            [ "Extend local environment with" <+> pretty (Mapping var (PrimInt i)) ]

        Detail_EnterUpdatable addr ->
            [ "Push a new update frame with the entered address" <+> pretty addr
            , "Overwrite the heap object at" <+> pretty addr <+> "with a black hole" ]

        Detail_ConUpdate con addrU ->
            [ "Trying to return" <+> pretty con <> ", but there is no return frame on the top of the stack"
            , "Update closure at" <+> pretty addrU <+> "given by the update frame with returned constructor"  ]

        Detail_PapUpdate updAddr ->
            [ "Not enough arguments on the stack"
            , "Try to reveal more arguments by performing the update for" <+> pretty updAddr ]

        Detail_ReturnIntCannotUpdate ->
            ["No closure has primitive type, so we cannot update one with a primitive int"]

        Detail_StackNotEmpty ->
            [ "The stack is not empty; the program terminated unexpectedly."
            , "The lack of a better description is a bug in the STG evaluator."
            , "Please report this to the project maintainers!" ]

        Detail_GarbageCollected algorithm deadAddrs movedAddrs -> mconcat
            [ [ "Algorithm: " <> string (T.unpack algorithm) ]
            , [ "Removed old address" <> pluralES deadAddrs <> ":" <+> pprAddrs deadAddrs ]
            , [ "Moved alive address" <> pluralES movedAddrs <> ":" <+> pprMoved movedAddrs
                | not (M.null movedAddrs) ]]
          where
            pprAddrs = pretty . commaSep . foldMap (\addr -> [pretty addr])
            pluralES [_] = ""
            pluralES _ = "es"

            pprMoved = commaSep . map (\(x, y) -> pretty (Mapping x y)) . M.assocs

        Detail_EnterBlackHole addr tick ->
            [ "Heap address" <+> pretty addr <+> "is a black hole, created in step" <+> pretty tick
            , "Entering a black hole means a thunk depends on its own evaluation"
            , "This is the functional equivalent of an infinite loop"
            , "GHC reports this condition as \"<<loop>>\"" ]

        Detail_UpdateClosureWithPrimitive ->
            [ "A closure never has primitive type, so it cannot be updated with a primitive value" ]

        Detail_BadConArity ->
            [ "Constructors always have to be fully applied." ] )

-- | A closure is a lambda form, together with the values of its free variables.
data Closure = Closure LambdaForm [Value]
    deriving (Eq, Ord, Show, Generic)

instance Pretty Closure where
    pretty (Closure lambdaForm []) = pretty lambdaForm
    pretty (Closure lambda freeVals) =
        prettyLambda prettyFree lambda
      where
        prettyFree vars = commaSep (zipWith (\k v -> pretty (Mapping k v)) vars freeVals)

-- | The heap stores closures addressed by memory location.
newtype Heap = Heap (Map MemAddr HeapObject)
    deriving (Eq, Ord, Show, Generic, Monoid)

instance Pretty Heap where
    pretty (Heap heap) = prettyMap heap

data HeapObject =
      HClosure Closure
    | Blackhole Integer
        -- ^ When an updatable closure is entered, it is overwritten by a
        -- black hole. This has two main benefits:
        --
        -- 1. Memory mentioned only in the closure is now ready to be collected,
        --    avoiding certain space leaks.
        -- 2. Entering a black hole means a thunk depends on itself, allowing
        --    the interpreter to catch some non-terminating computations with
        --    a useful error
        --
        -- To make the black hole a bit more transparent, it is tagged with
        -- the STG tick in which it was introduced. This tag is used only for
        -- display purposes.
    deriving (Eq, Ord, Show, Generic)

instance Pretty HeapObject where
    pretty ho = typeOf ho <+> pprHo ho
      where
        pprHo = \case
            HClosure closure -> align (pretty closure)
            Blackhole tick   -> "(from step" <+> integer tick <> ")"
        typeOf = closureType style . \case
            HClosure (Closure lf _free) -> pretty (classify lf)
            Blackhole _ -> "Blackhole"

instance NFData StgState
instance NFData StackFrame
instance NFData MemAddr
instance NFData Value
instance NFData Code
instance (NFData k, NFData v) => NFData (Mapping k v) where
    rnf (Mapping k v) = rnf k `seq` rnf v `seq` ()
instance NFData Globals
instance NFData Locals
instance NFData Info
instance NFData InfoShort
instance NFData StateTransition
instance NFData NotInScope
instance NFData StateError
instance NFData InfoDetail
instance NFData Closure
instance NFData Heap
instance NFData HeapObject
