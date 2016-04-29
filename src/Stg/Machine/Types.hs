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
    ArgumentFrame(..),
    ReturnFrame(..),
    UpdateFrame(..),
    MemAddr(..),
    Value(..),
    Code(..),
    Globals(..),
    Locals(..),
    Closure(..),
    Heap(..),

    -- * State information
    Info(..),
    InfoShort(..),
    InfoDetail(..),
    StateTransition(..),
    StateError(..),
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
    deriving (Eq, Ord, Show)

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
        , nest 4 (vsep
            [ "Stacks"
            , align (vsep
                [ "Arg:" <+> prettyStack (stgArgStack state)
                , "Ret:" <+> prettyStack (stgReturnStack state)
                , "Upd:" <+> prettyStack (stgUpdateStack state) ])])
        , nest 4 (vsep [ "Heap", pretty (stgHeap state)])
        , nest 4 (vsep [ "Globals", pretty (stgGlobals state)])
        , nest 4 ("Step:" <+> pretty (stgTicks state)) ])

instance PrettyAnsi StgState where
    prettyAnsi state = align (vsep
        [ headline colour "Code:" <+> prettyAnsi (stgCode state)
        , nest 4 (vsep
            [headline colour "Stacks"
            , align (vsep
                [ headline colour "Arg:" <+> prettyStackAnsi (stgArgStack state)
                , headline colour "Ret:" <+> prettyStackAnsi (stgReturnStack state)
                , headline colour "Upd:" <+> prettyStackAnsi (stgUpdateStack state) ])])
        , nest 4 (vsep [headline colour "Heap", prettyAnsi (stgHeap state)])
        , nest 4 (vsep [headline colour "Globals", prettyAnsi (stgGlobals state)])
        , nest 4 (headline colour "Step:" <+> pretty (stgTicks state)) ])

-- | Prettyprint a 'Stack'.
prettyStack :: Pretty a => Stack a -> Doc
prettyStack Empty = "(empty)"
prettyStack stack = (align . vsep) prettyFrames
  where
    prettyFrame frame i = "Frame" <+> int i <> ":" <+> align (pretty frame)
    prettyFrames = zipWith prettyFrame (toList stack) [1..]

-- | ANSI-prettyprint a 'Stack'.
prettyStackAnsi :: PrettyAnsi a => Stack a -> Doc
prettyStackAnsi Empty = "(empty)"
prettyStackAnsi stack = (align . vsep) prettyFrames
  where
    prettyFrame frame i = "Frame" <+> int i <> ":" <+> align (prettyAnsi frame)
    prettyFrames = zipWith prettyFrame (toList stack) (reverse [1..length stack])

-- | Argument frames store values on the argument stack, so that they can
-- later be retrieved when the calling function can be applied to them.
newtype ArgumentFrame = ArgumentFrame Value
    deriving (Eq, Ord, Show, Generic)

instance Pretty ArgumentFrame where
    pretty (ArgumentFrame val) = pretty val

instance PrettyAnsi ArgumentFrame where
    prettyAnsi (ArgumentFrame val) = prettyAnsi val

-- | Return frames are used when the scrutinee of a case expression is done
-- being evaluated, and the branch to continue on has to be decided.
data ReturnFrame = ReturnFrame Alts Locals
    deriving (Eq, Ord, Show, Generic)

instance Pretty ReturnFrame where
    pretty (ReturnFrame alts locals) =
        (align . vsep) [ fill 7 "Alts:"   <+> align (pretty alts)
                       , fill 7 "Locals:" <+> align (pretty locals) ]

instance PrettyAnsi ReturnFrame where
    prettyAnsi (ReturnFrame alts locals) =
        (align . vsep) [ headline colour (fill 7 "Alts:") <+> align (prettyAnsi alts)
                       , headline colour (fill 7 "Locals:") <+> align (prettyAnsi locals) ]

-- | Update frames store information about the machine's state before an
-- updateable closure was entered, so that they can help update it once it is
-- evaluated.
data UpdateFrame = UpdateFrame (Stack ArgumentFrame) (Stack ReturnFrame) MemAddr
    deriving (Eq, Ord, Show, Generic)

instance Pretty UpdateFrame where
    pretty (UpdateFrame upd ret addr) =
        (align . vsep)
             [ "Update address:" <+> pretty addr
             , "Arg:" <+> prettyStack upd
             , "Ret:" <+> prettyStack ret ]

instance PrettyAnsi UpdateFrame where
    prettyAnsi (UpdateFrame upd ret addr) =
        (align . vsep)
             [ headline colour "Update address:" <+> prettyAnsi addr
             , headline colour "Arg:" <+> prettyStackAnsi upd
             , headline colour "Ret:" <+> prettyStackAnsi ret]

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
data Code = Eval Expr Locals
          | Enter MemAddr
          | ReturnCon Constr [Value]
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

    | StateTransiton StateTransition
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
        StateTransiton t  -> "State transition:" <+> pretty t
        StateInitial      -> "Initial state"
        GarbageCollection -> "Garbage collection"

data StateTransition =
      Eval_FunctionApplication
    | Enter_NonUpdatableClosure
    | Eval_Let Rec
    | Eval_Case
    | Eval_AppC
    | ReturnCon_Match
    | ReturnCon_DefUnbound
    | ReturnCon_DefBound
    | Eval_Lit
    | Eval_LitApp
    | ReturnInt_Match
    | ReturnInt_DefBound
    | ReturnInt_DefUnbound
    | Eval_AppP
    | Enter_UpdatableClosure
    | ReturnCon_Update
    | Enter_PartiallyAppliedUpdate
    deriving (Eq, Ord, Show, Generic)

instance Pretty StateTransition where
    pretty = \case
        Eval_FunctionApplication     -> "Function application"
        Enter_NonUpdatableClosure    -> "Enter non-updatable closure"
        Eval_Let rec                 -> case rec of
                                            NonRecursive -> "let evaluation"
                                            Recursive -> "letrec evaluation"
        Eval_Case                    -> "case evaluation"
        Eval_AppC                    -> "Constructor application"
        ReturnCon_Match              -> "Algebraic constructor return, standard match"
        ReturnCon_DefUnbound         -> "Algebraic constructor return, unbound default match"
        ReturnCon_DefBound           -> "Algebraic constructor return, bound default match"
        Eval_Lit                     -> "Literal evaluation"
        Eval_LitApp                  -> "Literal application"
        ReturnInt_Match              -> "Primitive constructor return, standard match found"
        ReturnInt_DefBound           -> "Primitive constructor return, bound default match"
        ReturnInt_DefUnbound         -> "Primitive constructor return, unbound default match"
        Eval_AppP                    -> "Primitive function application"
        Enter_UpdatableClosure       -> "Enter updatable closure"
        ReturnCon_Update             -> "Update by constructor return"
        Enter_PartiallyAppliedUpdate -> "Enter partially applied closure"

instance PrettyAnsi StateTransition

data StateError =
      VariablesNotInScope [Var]
    | UpdatableClosureWithArgs
    | ReturnIntWithEmptyReturnStack
    | AlgReturnToPrimAlts
    | PrimReturnToAlgAlts
    | InitialStateCreationFailed
    deriving (Eq, Ord, Show, Generic)

-- | @[a,b,c]  ==>  a, b, c@
commaSep :: [Doc] -> Doc
commaSep = encloseSep mempty mempty (comma <> space)

instance Pretty StateError where
    pretty = \case
        VariablesNotInScope vars
            -> commaSep (map pretty vars) <+> "not in scope"
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

instance PrettyAnsi StateError where
    prettyAnsi = \case
        VariablesNotInScope vars
            -> commaSep (map prettyAnsi vars) <+> "not in scope"
        x -> pretty x

-- | Detailed information that may be useful to the user. Not used
-- programmatically.
newtype InfoDetail = InfoDetail [Text]
    deriving (Eq, Ord, Show, Generic)

instance Exts.IsList InfoDetail where
    type Item InfoDetail = Text
    toList = Exts.coerce
    fromList = Exts.coerce

instance Pretty InfoDetail where
    pretty (InfoDetail entries) = case entries of
        [] -> mempty
        es -> let -- TODO: rewrite this crap code
                  worded = map T.words es
                  concatted = map (hsep . map (text . T.unpack)) worded
                  bulleted = map ("  -" <+>) concatted
                  listed = align (vsep bulleted)
              in listed

-- | A closure is a lambda form, together with the values of its free variables.
data Closure = Closure LambdaForm [Value]
    deriving (Eq, Ord, Show, Generic)

-- | Like 'tupled', but comma-space separated.
tupled' :: [Doc] -> Doc
tupled' = encloseSep lparen rparen (comma <> space)

instance Pretty Closure where
    pretty (Closure (lambdaForm) []) = pretty lambdaForm
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
    prettyAnsi (Closure (lambdaForm) []) = prettyAnsi lambdaForm
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
newtype Heap = Heap (Map MemAddr Closure)
    deriving (Eq, Ord, Show, Monoid)

instance Pretty Heap where
    pretty (Heap heap) = prettyMap heap

instance PrettyAnsi Heap where
    prettyAnsi (Heap heap) = prettyAnsiMap heap
