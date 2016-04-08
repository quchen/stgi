-- | User-facing API to work with STG programs.
module Stg.Machine (
    initialState,
    stgStep,
) where



import qualified Data.Map          as M
import           Data.Maybe

import           Stg.Language
import           Stg.Machine.Env
import qualified Stg.Machine.Heap  as H
import           Stg.Machine.Step
import           Stg.Machine.Types



-- | Create a suitable initial state for an STG.
initialState
    :: Var -- ^ Main
    -> Program
    -> StgState
initialState mainVar (Program binds) = StgState
    { stgCode        = Eval (AppF mainVar []) mempty
    , stgArgStack    = mempty
    , stgReturnStack = mempty
    , stgUpdateStack = mempty
    , stgHeap        = heap
    , stgGlobals     = globals
    , stgTicks       = 0 }
  where
    globalVars :: [Var]
    globalVals :: [LambdaForm]
    (globalVars, globalVals) = let Binds b = binds in unzip (M.assocs b)

    globals :: Globals
    globals = makeGlobals (zipWith (\n a -> (n, Addr a)) globalVars addrs)

    addrs :: [MemAddr]
    heap :: Heap
    (addrs, heap) = H.allocMany (map liftClosure globalVals) mempty

    -- TODO: Unify this with the other liftClosure
    liftClosure :: LambdaForm -> Closure
    liftClosure lf@(LambdaForm free _ _ _) =
        let freeVals :: [Value]
            freeVals = fromMaybe (error "FOOBAR-MAIN")
                                 (traverse (globalVal globals) free)
        in Closure lf freeVals
