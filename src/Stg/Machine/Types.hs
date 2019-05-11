{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Types used in the execution of the STG machine.
module Stg.Machine.Types (
    StgState(..),
    StgiAnn(..),
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
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import qualified Data.Semigroup            as Semigroup
import           Data.Set                  (Set)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc
import           GHC.Generics
import           Text.Printf

import Data.Stack
import Stg.Language
import Stg.Language.Prettyprint
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
        -- ^ A counter, used to generte fresh variable names and for orientation
        -- purposes.

    , stgInfo :: Info
        -- ^ Information about the current state
    }
    deriving (Eq, Ord, Show, Generic)

-- | Local re-definition to avoid cyclic import with the Heap module
heapSize :: Heap -> Int
heapSize (Heap h) = length h

instance PrettyStgi StgState where
    prettyStgi state = align (vsep
        [ annotate (StateAnn Headline) "Code:" <+> prettyStgi (stgCode state)
        , nest 4 (vsep [annotate (StateAnn Headline) "Stack", prettyStack (stgStack state) ])
        , nest 4 (vsep [annotate (StateAnn Headline) "Heap" <> " (" <> pretty (heapSize (stgHeap state)) <+> "entries)"
                       , prettyStgi (stgHeap state) ])
        , nest 4 (vsep [annotate (StateAnn Headline) "Globals", prettyStgi (stgGlobals state)])
        , nest 4 (annotate (StateAnn Headline) "Step:" <+> pretty (stgSteps state)) ])

-- | Prettyprint a 'Stack'.
prettyStack :: PrettyStgi a => Stack a -> Doc StgiAnn
prettyStack Empty = "(empty)"
prettyStack stack = (align . vsep) prettyFrames
  where
    prettyFrame frame i = hsep
        [ annotate (StateAnn Headline) (pretty i <> ".")
        , align (prettyStgi frame) ]
    prettyFrames = zipWith prettyFrame (toList stack) (reverse [1..length stack])

-- | Stack frames unify arguments, returns, and updates.
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

instance PrettyStgi StackFrame where
    prettyStgi = \case
        ArgumentFrame val -> annotate (StateAnn StackFrameType) "Arg" <+> prettyStgi val
        ReturnFrame alts locals -> annotate (StateAnn StackFrameType) "Ret" <+>
            (align . vsep) [ fill 7 (annotate (StateAnn Headline) "Alts:")   <+> align (prettyStgi alts)
                           , fill 7 (annotate (StateAnn Headline) "Locals:") <+> align (prettyStgi locals) ]
        UpdateFrame addr -> annotate (StateAnn StackFrameType) "Upd" <+> prettyStgi addr

-- | A memory address.
newtype MemAddr = MemAddr Int
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance PrettyStgi MemAddr where
    prettyStgi (MemAddr addr) = annotate (StateAnn Address) ("0x" <> annotate (StateAnn AddressCore) (hexAddr addr))
      where
        hexAddr = pretty . (printf "%02x" :: Int -> String)

-- | A value of the STG machine.
data Value = Addr MemAddr | PrimInt Integer
    deriving (Eq, Ord, Show, Generic)

instance PrettyStgi Value where
    prettyStgi = \case
        Addr addr -> prettyStgi addr
        PrimInt i -> prettyStgi (Literal i)

-- | The different code states the STG can be in.
data Code =
          -- | Evaluate an expression within a local environment
          Eval Expr Locals

          -- | Load the closure at a certain heap address
        | Enter MemAddr

          -- | Sub-computation terminated with an algebraic 'Constr'uctor
        | ReturnCon Constr [Value]

          -- | Sub-computation terminated with a primitive integer
        | ReturnInt Integer
    deriving (Eq, Ord, Show, Generic)

instance PrettyStgi Code where
    prettyStgi = \case
        Eval expr locals -> (align . vsep)
            [ "Eval" <+> prettyStgi expr
            , annotate (StateAnn Headline) "Locals:" <+> prettyStgi locals ]
        Enter addr -> "Enter" <+> prettyStgi addr
        ReturnCon constr args -> "ReturnCon" <+> prettyStgi constr <+> hsep (map prettyStgi args)
        ReturnInt i -> "ReturnInt" <+> prettyStgi (Literal i)

-- | A single key -> value association.
--
-- Used to make 2-tuples to be inserted  into association maps clearer.
data Mapping k v = Mapping k v
    deriving (Eq, Ord, Show, Generic)

instance (PrettyStgi k, PrettyStgi v) => PrettyStgi (Mapping k v) where
    prettyStgi (Mapping k v) = prettyStgi k <+> "->" <+> prettyStgi v

-- | Prettyprint a 'Map', @key -> value@.
prettyMap :: (PrettyStgi k, PrettyStgi v) => Map k v -> Doc StgiAnn
prettyMap m | M.null m = "(empty)"
prettyMap m = (align . vsep) [ prettyStgi (Mapping k v) | (k,v) <- M.assocs m ]

-- | The global environment consists of the mapping from top-level definitions
-- to their respective values.
newtype Globals = Globals (Map Var Value)
    deriving (Eq, Ord, Show, Semigroup.Semigroup, Monoid, Generic)

instance PrettyStgi Globals where
    prettyStgi (Globals globals) = prettyMap globals

-- | The global environment consists if the mapping from local definitions
-- to their respective values.
newtype Locals = Locals (Map Var Value)
    deriving (Eq, Ord, Show, Semigroup.Semigroup, Monoid, Generic)

instance PrettyStgi Locals where
    prettyStgi (Locals locals) = prettyMap locals

-- | User-facing information about the current state of the STG.
data Info = Info InfoShort [InfoDetail]
    deriving (Eq, Ord, Show, Generic)

instance PrettyStgi Info where
    prettyStgi = \case
        Info short []      -> prettyStgi short
        Info short details -> vsep [prettyStgi short, vsep (map prettyStgi details)]

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

instance PrettyStgi InfoShort where
    prettyStgi = \case
        HaltedByPredicate -> "Halting predicate held"
        NoRulesApply      -> "No further rules apply"
        MaxStepsExceeded  -> "Maximum number of steps exceeded"
        StateError err    -> "Errorenous state:" <+> prettyStgi err
        StateTransition t -> prettyStgi t
        StateInitial      -> "Initial state"
        GarbageCollection -> "Garbage collection"

-- | Classifies which rule has been applied in order to reach the current state.
data StateTransition =
      Rule1_Eval_FunctionApplication
    | Rule2_Enter_NonUpdatableClosure
    | Rule3_Eval_Let Rec
    | Rule4_Eval_Case
    | Rule5_Eval_AppC
    | Rule6_ReturnCon_Match
    | Rule7_ReturnCon_DefUnbound
    | Rule8_ReturnCon_DefBound
    | Rule9_Lit
    | Rule10_LitApp
    | Rule11_ReturnInt_Match
    | Rule12_ReturnInt_DefBound
    | Rule13_ReturnInt_DefUnbound
    | Rule14_Eval_AppP
    | Rule15_Enter_UpdatableClosure
    | Rule16_ReturnCon_Update
    | Rule17_Enter_PartiallyAppliedUpdate
    | Rule17a_Enter_PartiallyAppliedUpdate
    | Rule1819_Eval_Case_Primop_Shortcut
    deriving (Eq, Ord, Show, Generic)

instance PrettyStgi StateTransition where
    prettyStgi = \case
        Rule1_Eval_FunctionApplication       -> "Function application"
        Rule2_Enter_NonUpdatableClosure      -> "Enter non-updatable closure"
        Rule3_Eval_Let NonRecursive          -> "Let evaluation"
        Rule3_Eval_Let Recursive             -> "Letrec evaluation"
        Rule4_Eval_Case                      -> "Case evaluation"
        Rule5_Eval_AppC                      -> "Constructor application"
        Rule6_ReturnCon_Match                -> "Algebraic constructor return, standard match"
        Rule7_ReturnCon_DefUnbound           -> "Algebraic constructor return, unbound default match"
        Rule8_ReturnCon_DefBound             -> "Algebraic constructor return, bound default match"
        Rule9_Lit                            -> "Literal evaluation"
        Rule10_LitApp                        -> "Literal application"
        Rule11_ReturnInt_Match               -> "Primitive constructor return, standard match found"
        Rule12_ReturnInt_DefBound            -> "Primitive constructor return, bound default match"
        Rule13_ReturnInt_DefUnbound          -> "Primitive constructor return, unbound default match"
        Rule14_Eval_AppP                     -> "Primitive function application"
        Rule15_Enter_UpdatableClosure        -> "Enter updatable closure"
        Rule16_ReturnCon_Update              -> "Update by constructor return"
        Rule17_Enter_PartiallyAppliedUpdate  -> "Enter partially applied closure"
        Rule17a_Enter_PartiallyAppliedUpdate -> "Enter partially applied closure"
        Rule1819_Eval_Case_Primop_Shortcut   -> "Case evaluation of primop: taking a shortcut"

-- | Type safety wrapper to report variables that were not in scope.
newtype NotInScope = NotInScope [Var]
    deriving (Eq, Ord, Show, Generic, Semigroup.Semigroup, Monoid)

instance PrettyStgi NotInScope where
    prettyStgi (NotInScope vars) = commaSep (map prettyStgi vars)

-- | Like 'StateTransition', but for invalid transitions.
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
    | BadConArity Int Int -- ^ scrutinee arity, pattern arity
    deriving (Eq, Ord, Show, Generic)

instance PrettyStgi StateError where
    prettyStgi = \case
        VariablesNotInScope notInScope -> prettyStgi notInScope <+> "not in scope"
        UpdatableClosureWithArgs       -> "Closures with non-empty argument lists are never updatable"
        ReturnIntWithEmptyReturnStack  -> "ReturnInt state with empty return stack"
        AlgReturnToPrimAlts            -> "Algebraic constructor return to primitive alternatives"
        PrimReturnToAlgAlts            -> "Primitive return to algebraic alternatives"
        InitialStateCreationFailed     -> "Initial state creation failed"
        EnterBlackhole                 -> "Entering black hole"
        UpdateClosureWithPrimitive     -> "Update closure with primitive value"
        NonAlgPrimScrutinee            -> "Non-algebraic/primitive case scrutinee"
        DivisionByZero                 -> "Division by zero"
        BadConArity retArity altArity  -> "Return" <+> pprArity retArity
                                          <+> "constructor to"
                                          <+> pprArity altArity
                                          <+> "alternative"



-- | Prettyprint an arity.
--
-- >>> let ppr = Data.Text.IO.putStrLn . Stg.Language.Prettyprint.renderPlain
-- >>> for_ [0..5] (ppr . pprArity)
-- nullary
-- unary
-- binary
-- ternary
-- 4-ary
-- 5-ary
pprArity :: Int -> Doc ann
pprArity = \case
    0 -> "nullary"
    1 -> "unary"
    2 -> "binary"
    3 -> "ternary"
    n -> pretty n <> "-ary"

-- | Used to store meta-information about state transitions in order to be
-- rendered as a helpful hint.
data InfoDetail =
      Detail_FunctionApplication Var [Atom]
    | Detail_UnusedLocalVariables [Var] Locals
    | Detail_EnterNonUpdatable MemAddr [Mapping Var Value]
    | Detail_EvalLet [Var] [MemAddr]
    | Detail_EvalCase
    | Detail_ReturnCon_Match Constr [Mapping Var Value]
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

-- | Prefix all contained documents with a bullet symbol.
bulletList :: [Doc ann] -> Doc ann
bulletList = align . vsep . map (("  - " <>) . align)

pluralS :: [a] -> Doc ann
pluralS [_] = "s"
pluralS _ = ""

instance PrettyStgi InfoDetail where
    prettyStgi :: InfoDetail -> Doc StgiAnn
    prettyStgi items = bulletList (case items of
        Detail_FunctionApplication val [] ->
            ["Inspect value" <+> prettyStgi val]
        Detail_FunctionApplication function args ->
            [ "Apply function"
              <+> prettyStgi function
              <+> "to argument" <> pluralS args
              <+> commaSep (map prettyStgi args) ]

        Detail_UnusedLocalVariables usedVars (Locals locals) ->
            let used = M.fromList [ (var, ()) | var <- usedVars ]
                unused = locals `M.difference` used
                pprDiscardedBind var val = [prettyStgi var <+> "(" <> prettyStgi val <> ")"]
            in ["Unused local variable" <> pluralS (M.toList unused) <+> "discarded:"
                <+> case unused of
                    [] -> "(none)"
                    _  -> commaSep (M.foldMapWithKey pprDiscardedBind unused) ]

        Detail_EnterNonUpdatable addr args ->
            [ "Enter closure at" <+> prettyStgi addr
            , if null args
                then prettyStgi addr <+> "does not take any arguments, so no frames are popped"
                else hang 4 (vsep
                        [ "Extend local environment with mappings from bound values to argument frame addresses:"
                        , commaSep (foldMap (\arg -> [prettyStgi arg]) args) ])]

        Detail_EvalLet vars addrs ->
            [ hsep
                [ "Local environment extended by"
                , commaSep (foldMap (\var -> [prettyStgi var]) vars) ]
            , hsep
                [ "Allocate new closure" <> pluralS vars <+> "at"
                , commaSep (zipWith (\var addr -> prettyStgi addr <+> "(" <> prettyStgi var <> ")") vars addrs)
                , "on the heap" ]]

        Detail_EvalCase ->
            ["Save alternatives and local environment as a stack frame"]

        Detail_ReturnCon_Match con args ->
            ["Pattern" <+> prettyStgi (AppC con (map (\(Mapping a _) -> AtomVar a) args)) <+> "matches, follow its branch"
            , if null args
                then mempty
                else hang 4 (vsep
                        [ "Extend local environment with matched pattern variables' values:"
                        , commaSep (foldMap (\arg -> [prettyStgi arg]) args) ])]

        Detail_ReturnConDefBound var addr ->
            [ "Allocate closure at" <+> prettyStgi addr <+> "for the bound value"
            , "Extend local environment with" <+> prettyStgi (Mapping var addr) ]

        Detail_ReturnIntDefBound var i ->
            [ "Extend local environment with" <+> prettyStgi (Mapping var (PrimInt i)) ]

        Detail_EnterUpdatable addr ->
            [ "Push a new update frame with the entered address" <+> prettyStgi addr
            , "Overwrite the heap object at" <+> prettyStgi addr <+> "with a black hole" ]

        Detail_ConUpdate con addrU ->
            [ "Trying to return" <+> prettyStgi con <> ", but there is no return frame on the top of the stack"
            , "Update closure at" <+> prettyStgi addrU <+> "given by the update frame with returned constructor"  ]

        Detail_PapUpdate updAddr ->
            [ "Not enough arguments on the stack"
            , "Try to reveal more arguments by performing the update for" <+> prettyStgi updAddr ]

        Detail_ReturnIntCannotUpdate ->
            ["No closure has primitive type, so we cannot update one with a primitive int"]

        Detail_StackNotEmpty ->
            [ "The stack is not empty; the program terminated unexpectedly."
            , "The lack of a better description is a bug in the STG evaluator."
            , "Please report this to the project maintainers!" ]

        Detail_GarbageCollected algorithm deadAddrs movedAddrs -> mconcat
            [ [ "Algorithm: " <> pretty algorithm ]
            , [ "Removed old address" <> pluralES deadAddrs <> ":" <+> pprAddrs deadAddrs ]
            , [ "Moved alive address" <> pluralES movedAddrs <> ":" <+> pprMoved movedAddrs
                | not (M.null movedAddrs) ]]
          where
            pprAddrs = commaSep . foldMap (\addr -> [prettyStgi addr])
            pluralES [_] = ""
            pluralES _ = "es"

            pprMoved = commaSep . map (\(x, y) -> prettyStgi (Mapping x y)) . M.assocs

        Detail_EnterBlackHole addr tick ->
            [ "Heap address" <+> prettyStgi addr <+> "is a black hole, created in step" <+> pretty tick
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

instance PrettyStgi Closure where
    prettyStgi (Closure lambdaForm []) = prettyStgi lambdaForm
    prettyStgi (Closure lambda freeVals) =
        prettyLambda prettyFree lambda
      where
        prettyFree vars = commaSep (zipWith (\k v -> prettyStgi (Mapping k v)) vars freeVals)

-- | The heap stores closures addressed by memory location.
newtype Heap = Heap (Map MemAddr HeapObject)
    deriving (Eq, Ord, Show, Generic, Semigroup.Semigroup, Monoid)

instance PrettyStgi Heap where
    prettyStgi (Heap heap) = prettyMap heap

-- | Heap objects are what is stored on the heap. The most common and also most
-- important one are closures.
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

instance PrettyStgi HeapObject where
    prettyStgi ho = typeOf ho <+> pprHo ho
      where
        pprHo = \case
            HClosure closure -> align (prettyStgi closure)
            Blackhole tick   -> "(from step" <+> pretty tick <> ")"
        typeOf = annotate (StateAnn ClosureType) . \case
            HClosure (Closure lf _free) -> prettyStgi (classify lf)
            Blackhole _ -> "Blackhole"

instance NFData StgState
instance NFData StackFrame
instance NFData MemAddr
instance NFData Value
instance NFData Code
instance (NFData k, NFData v) => NFData (Mapping k v) where
    rnf (Mapping k v) = rnf (k,v)
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
