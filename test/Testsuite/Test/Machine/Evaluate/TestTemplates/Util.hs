{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Test.Machine.Evaluate.TestTemplates.Util (
    (===>),
    PrettyprinterDict(..),
) where


import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import Stg.Language
import Stg.Language.Prettyprint
import Stg.Machine.Env
import Stg.Machine.Heap         as H
import Stg.Machine.Types
import Stg.Util



-- | Build a state predicate that asserts that a certain 'Var' maps to
-- a 'LambdaForm' in the heap.
(===>) :: Var -> LambdaForm -> StgState -> Bool
var ===> lambdaForm = \state -> case varLookup state var of
    VarLookupClosure (Closure lf _) -> lf == lambdaForm
    _otherwise                      -> False

-- | Used as the result of 'varLookup'.
data VarLookupResult =
    VarLookupError Text
    | VarLookupPrim Integer
    | VarLookupClosure Closure
    | VarLookupBlackhole
    deriving (Eq, Ord, Show)

-- | Look up the value of a 'Var' on the 'Heap' of a 'StgState'.
varLookup :: StgState -> Var -> VarLookupResult
varLookup state var =
    case globalVal (stgGlobals state) (AtomVar var) of
        Failure (NotInScope notInScope) -> VarLookupError (T.intercalate ", " (map (\(Var v) -> v) notInScope) <> " not in global scope")
        Success (Addr addr) -> case H.lookup addr (stgHeap state) of
            Just (HClosure closure)  -> VarLookupClosure closure
            Just (Blackhole _bhTick) -> VarLookupBlackhole
            Nothing                  -> VarLookupError "not found on heap"
        Success (PrimInt i) -> VarLookupPrim i

data PrettyprinterDict = PrettyprinterDict (forall a. PrettyAnsi a => a -> Text)
                                           (forall a. PrettyAnsi a => a -> Doc)
