{-# LANGUAGE OverloadedLists #-}

-- | User-facing API to work with STG programs.
module Stg.Machine (
    initialState,

    -- * Evaluation
    evalStep,
    evalUntil,
    evalsUntil,
    terminated,
    HaltIf(..),
    RunForSteps(..),

    -- * Garbage collection
    garbageCollect,
    PerformGc(..),
    GarbageCollectionAlgorithm,
    triStateTracing,
    twoSpaceCopying,
) where



import           Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE

import Stg.Language
import Stg.Machine.Evaluate
import Stg.Machine.GarbageCollection
import Stg.Machine.Types



-- | Create a suitable initial state for an STG.
initialState
    :: Var -- ^ Main
    -> Program
    -> StgState
initialState mainVar (Program binds) = initializedState
  where
    -- In order to avoid code duplication, we create the initial state by
    -- packing the entire program in a "letrec <topLevelDefs> in <main>".
    -- Evaluating this step once allocates everything as desired; the resulting
    -- state is precisely the initial state we want, except that the definitions
    -- are stored in the local environment, and not in the global. We therefore
    -- copy that over, and we're done.
    --
    -- Avoiding the copying altogether would have its advantages: no manual
    -- fiddling, and once main is done, everything could be garbage collected.
    -- Unfortunately, GC and rules rely on the existence of a global
    -- environment, so we *have* to fill it.
    dummyLetInitial = StgState
        { stgCode    = Eval (Let Recursive binds (AppF mainVar [])) mempty
        , stgStack   = mempty
        , stgHeap    = mempty
        , stgGlobals = mempty
        , stgSteps   = 0
        , stgInfo    = Info StateInitial [] }
    initializedState = case evalStep dummyLetInitial of
        state | terminated state -> state
        state@StgState
            { stgCode = Eval (AppF _mainVar []) (Locals locals) }
          -> state
            { stgCode    = Eval (AppF mainVar []) mempty
            , stgSteps   = 0
            , stgGlobals = Globals locals
            , stgInfo    = Info StateInitial [] }
        badState -> badState
            { stgInfo = Info (StateError InitialStateCreationFailed) [] }



-- | Predicate to decide whether the machine should halt.
data RunForSteps =
      RunIndefinitely -- ^ Do not terminate based on the number of steps
    | RunForMaxSteps Integer

-- | Predicate to decide whether the machine should halt.
newtype HaltIf = HaltIf (StgState -> Bool)

-- | Decide whether garbage collection should be attempted, and with which
-- algorithm.
newtype PerformGc = PerformGc (StgState -> Maybe GarbageCollectionAlgorithm)

-- | Evaluate the STG until a predicate holds, aborting if the maximum number of
-- steps are exceeded.
--
-- @
-- 'last' ('evalsUntil' ...) ≡ 'evalUntil'
-- @
evalUntil
    :: RunForSteps -- ^ Maximum number of steps allowed
    -> HaltIf      -- ^ Halting decision function
    -> PerformGc   -- ^ Condition under which to perform GC
    -> StgState    -- ^ Initial state
    -> StgState    -- ^ Final state
evalUntil runForSteps halt performGc state
    = NE.last (evalsUntil runForSteps halt performGc state)

data AttemptGc = GcPossible | SkipGc
    deriving (Eq, Ord, Show)

-- | Evaluate the STG, and record all intermediate states.
--
-- * Stop when a predicate holds.
-- * Stop if the maximum number of steps are exceeded.
-- * Perform GC on every step.
--
-- @
-- 'evalsUntil' ≈ 'unfoldr' 'evalUntil'
-- @
evalsUntil
    :: RunForSteps       -- ^ Maximum number of steps allowed
    -> HaltIf            -- ^ Halting decision function
    -> PerformGc         -- ^ Condition under which to perform GC
    -> StgState          -- ^ Initial state
    -> NonEmpty StgState -- ^ Initial state plus intermediate states
evalsUntil runForSteps (HaltIf haltIf) (PerformGc performGc)
  = step SkipGc
  where
    terminateWith :: a -> NonEmpty a
    terminateWith = pure

    isInitialOrTransition state = case stgInfo state of
        Info StateTransition{} _ -> True
        Info StateInitial      _ -> True
        _otherwise               -> False

    -- Max steps exceeded
    step _ state
        | RunForMaxSteps maxSteps <- runForSteps
        , stgSteps state >= maxSteps
        = terminateWith (state { stgInfo = Info MaxStepsExceeded [] })

    -- Halting predicate triggered
    step _ state
        | haltIf state
        = terminateWith (state { stgInfo = Info HaltedByPredicate [] })

    -- Normal state transition, attempt GC
    step gcPossible state
        | isInitialOrTransition state
        , gcPossible == GcPossible
        , Just gcAlgorithm <- performGc state
        = case garbageCollect gcAlgorithm state of
            stateGc@StgState{stgInfo = Info GarbageCollection _} ->
                state <| stateGc <| step SkipGc (evalStep stateGc)
            _otherwise -> state <| step GcPossible (evalStep state)

    -- Normal state transition, no GC
    step _ state
        | isInitialOrTransition state
        = state <| step GcPossible (evalStep state)

    -- Garbage collection triggered last time
    step _ state@StgState{ stgInfo = Info GarbageCollection _ }
        = state <| step SkipGc (evalStep state)

    -- Anything else: terminateWith in the current state
    step _ state = terminateWith state

-- | Check whether a state is terminal.
terminated :: StgState -> Bool
terminated StgState{stgInfo = Info info _} = case info of
    StateTransition{}   -> False
    StateInitial{}      -> False
    GarbageCollection{} -> False
    _otherwise          -> True
