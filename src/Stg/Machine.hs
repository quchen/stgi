{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | User-facing API to work with STG programs.
module Stg.Machine (
    initialState,
    evalStep,
    evalUntil,
    evalsUntil,
    terminated,
    HaltIf(..),

    -- * Garbage collection
    garbageCollect,
    PerformGc(..)
) where



import           Data.Monoid
import qualified Data.Text                     as T

import           Stg.Language
import           Stg.Language.Prettyprint
import           Stg.Machine.Evaluate
import           Stg.Machine.GarbageCollection (Alive (..), Dead (..))
import qualified Stg.Machine.GarbageCollection as GC
import qualified Stg.Machine.Heap              as H
import           Stg.Machine.Types



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
        { stgCode        = Eval (Let Recursive binds (AppF mainVar [])) mempty
        , stgArgStack    = mempty
        , stgReturnStack = mempty
        , stgUpdateStack = mempty
        , stgHeap        = mempty
        , stgGlobals     = mempty
        , stgTicks       = 0
        , stgInfo        = Info StateInitial [] }
    initializedState = case evalStep dummyLetInitial of
        state | terminated state -> state
        state@StgState
            { stgCode = Eval (AppF _mainVar []) (Locals locals) }
          -> state
            { stgCode    = Eval (AppF mainVar []) mempty
            , stgTicks   = 0
            , stgGlobals = Globals locals
            , stgInfo    = Info StateInitial [] }
        badState -> badState
            { stgInfo = Info (StateError "Initial state creation failed") [] }


garbageCollect :: StgState -> StgState
garbageCollect state
  = let (Dead deadHeap, Alive cleanHeap) = GC.garbageCollect state
        garbageAddresses = (T.intercalate ", " . foldMap (\addr -> [prettyprint addr]) . H.addresses) deadHeap
        garbageWasCollected = H.size deadHeap > 0

    in if garbageWasCollected
            then state { stgHeap  = cleanHeap
                       , stgTicks = stgTicks state + 1
                       , stgInfo  = Info GarbageCollection
                                         ["Removed addresses: " <> garbageAddresses] }
            else state


-- | Predicate to decide whether the machine should halt.
newtype HaltIf = HaltIf (StgState -> Bool)

-- | Predicate to decide whether a garbage collection should be attempted
newtype PerformGc = PerformGc (StgState -> Bool)

-- | Evaluate the STG until a predicate holds, aborting if the maximum number of
-- steps are exceeded.
--
-- @
-- 'last' ('evalsUntil' ...) ≡ 'evalUntil'
-- @
evalUntil
    :: Integer   -- ^ Maximum number of steps allowed
    -> HaltIf    -- ^ Halting decision function
    -> PerformGc -- ^ Condition under which to perform GC
    -> StgState  -- ^ Initial state
    -> StgState  -- ^ Final state
evalUntil maxSteps halt performGc state
    = last (evalsUntil maxSteps halt performGc state)

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
    :: Integer    -- ^ Maximum number of steps allowed
    -> HaltIf     -- ^ Halting decision function
    -> PerformGc  -- ^ Condition under which to perform GC
    -> StgState   -- ^ Initial state
    -> [StgState] -- ^ All intermediate states
evalsUntil maxSteps (HaltIf haltIf) (PerformGc performGc) = go False
  where
    terminate = (:[])
    go attemptGc = \case

        state@StgState{ stgTicks = ticks } | ticks >= maxSteps
            -> terminate (state { stgInfo = Info MaxStepsExceeded [] })

        state | haltIf state
            -> terminate (state { stgInfo = Info HaltedByPredicate [] })

        state@StgState{ stgInfo = Info (StateTransiton{}) _ }
            | attemptGc && performGc state -> case garbageCollect state of
                stateGc@StgState{stgInfo = Info GarbageCollection _} ->
                    state : stateGc : go False (evalStep stateGc)
                _otherwise -> state : go True (evalStep state)
            | otherwise -> state : go True (evalStep state)

        state@StgState{ stgInfo = Info StateInitial _ }
            | attemptGc && performGc state -> case garbageCollect state of
                stateGc@StgState{stgInfo = Info GarbageCollection _} ->
                    state : stateGc : go False (evalStep stateGc)
                _otherwise -> state : go True (evalStep state)
            | otherwise -> state : go True (evalStep state)

        state@StgState{ stgInfo = Info GarbageCollection _ }
            -> state : go False (evalStep state)

        state
            -> terminate state

-- | Check whether a state is terminal.
terminated :: StgState -> Bool
terminated StgState{stgInfo = Info info _} = case info of
    StateTransiton{}    -> False
    StateInitial{}      -> False
    GarbageCollection{} -> False
    _otherwise          -> True
