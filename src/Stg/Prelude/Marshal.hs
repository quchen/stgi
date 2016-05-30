{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Convert Haskell values to STG values and back.
module Stg.Prelude.Marshal (
    ToStg(..),
    FromStg(..),
) where



import Data.Maybe
import Data.Monoid

import           Stg.Language
import           Stg.Machine.Env        (globalVal)
import qualified Stg.Machine.Heap       as H
import           Stg.Machine.Types
import           Stg.Parser.QuasiQuoter
import qualified Stg.Prelude.List       as Stg
import           Stg.Util



-- | Look up the value of a global variable.
--
-- Instances of this class should have a corresponding 'ToStg' instance to
-- inject a value into the program, with the two being inverse to each other (up
-- to forcing the generated thunks).
class FromStg value where
    fromStg
        :: StgState
        -> Var -- ^ Name of the global, e.g. @main@
        -> Either FromStgError value

data FromStgError =
      TypeMismatch        -- ^ e.g. asking for an @Int#@ at an address
                          --   that contains a @Cons@
    | NotInNormalForm     -- ^ Tried retrieving a thunk
    | BadConstructor      -- ^ e.g. @Cons x y z@, @Int#@ not applied
                          --   to a primitive int
    | NotFound NotInScope -- ^ A variable lookup unsuccessful
    | AddrNotOnHeap

instance FromStg Integer where
    fromStg stgState var = case globalVal (stgGlobals stgState) (AtomVar var) of
        Failure _ -> Left (NotFound (NotInScope [var]))
        Success val -> case val of
            PrimInt i -> Right i
            Addr addr -> case H.lookup addr (stgHeap stgState) of
                Nothing -> Left AddrNotOnHeap
                Just heapObject -> case heapObject of
                    Blackhole{}      -> Left NotInNormalForm
                    HClosure closure -> inspectClosure closure
      where
        inspectClosure = \case
            Closure (LambdaForm _ _ args _) _
                | not (null args) -> Left BadConstructor
            Closure (LambdaForm _ _ _ (AppC "Int#" intArgs)) _
                | not (intArgs `lengthEquals` 1) -> Left BadConstructor
            Closure (LambdaForm [] _ _ (AppC "Int#" [AtomLit (Literal i)])) []
                -> Right i
            Closure (LambdaForm freeVars _ _ (AppC "Int#" [AtomVar x])) freeVals
                -> case listToMaybe [ fVal | (fVar,fVal) <- zip freeVars freeVals, fVar == x ] of
                    Just (PrimInt i) -> Right i
                    Just (Addr _)    -> Left BadConstructor
                    Nothing          -> Left (NotFound (NotInScope [x]))
            Closure _ _
                -> Left TypeMismatch

lengthEquals :: [a] -> Int -> Bool
lengthEquals [] 0 = True
lengthEquals (_:xs) !n = lengthEquals xs n
lengthEquals _ _ = False

-- | Convert a Haskell value to an STG binding.
--
-- Instances of this class should have a corresponding 'FromStg' instance to
-- retrieve a value fom the program, with the two being inverse to each other
-- (up to forcing the generated thunks).
class ToStg value where
    toStg
        :: Var -- ^ Name of the binding
        -> value
        -> Program

-- | >>> prettyprintPlain (toStg "one" 1)
-- "one = \\ -> Int# 1#"
instance ToStg Integer where
    toStg name i = Program (Binds [(name, LambdaForm [] NoUpdate []
        (AppC (Constr "Int#") [AtomLit (Literal i)]) )])

instance ToStg a => ToStg [a] where
    toStg name [] = Stg.nil <> Program (Binds [(name, [stg| \ -> nil |])])
    toStg name _ = _implement_me

















{-

-- | Generate a list of numbers.
--
-- Also demonstrate nicely how much overhead seemingly simple lists have.
--
-- >>> let ppr ast = T.putStrLn (prettyprintPlain ast)
-- >>> ppr (listOfNumbers "list" [1, -2, 3])
-- list = \ => letrec
--                 int_1 = \ -> Int# 1#;
--                 int_3 = \ -> Int# 3#;
--                 int_neg2 = \ -> Int# -2#;
--                 list_ix0_int_1 = \(int_1 list_ix1_int_neg2) -> Cons int_1 list_ix1_int_neg2;
--                 list_ix1_int_neg2 = \(int_neg2 list_ix2_int_3) -> Cons int_neg2 list_ix2_int_3;
--                 list_ix2_int_3 = \(int_3) -> Cons int_3 nil
--             in list_ix0_int_1;
-- nil = \ -> Nil
listOfNumbers
    :: T.Text      -- ^ Name of the list in the STG program
    -> [P.Integer] -- ^ Entries
    -> Program
-- TODO: The paper mentions a more efficient construction of literal source
-- lists that is "usually superior".
listOfNumbers name [] = nil <> Program (Binds [(Var name, [lambdaForm| \ -> nil |])])
listOfNumbers name ints = nil <>
    Program (Binds [
        ( Var name
        , LambdaForm [] Update []
            (Let Recursive
                (Binds (M.fromList (intBinds <> listBinds)))
                (AppF (Var (listBindName 0 (P.head ints))) []) ))])
  where
    intBinds = P.map intBind ints
    listBinds = P.zipWith3 listBind
                           [0..]
                           ints
                           (P.zipWith listBindName [1..] (P.tail ints) <> ["nil"])

    listBind ix i tailName =
        ( Var (listBindName ix i)
        , LambdaForm ([Var (intName i)] <> [ Var tailName | tailName P./= "nil"])
                     NoUpdate -- Standard constructors are not updatable
                     []
                     (AppC (Constr "Cons")
                           [AtomVar (Var (intName i)), AtomVar (Var tailName)] ))
    listBindName :: P.Integer -> P.Integer -> Text
    listBindName ix i = "list_ix" <> show' ix <> "_" <> intName i

    intBind :: P.Integer -> (Var, LambdaForm)
    intBind i =
        ( Var (intName i)
        , LambdaForm [] NoUpdate []
                     (AppC (Constr "Int#") [AtomLit (Literal i)]))

    intName :: P.Integer -> T.Text
    intName i = "int_" <> sign <> show' (P.abs i)
      where
        sign | i P.< 0 = "neg"
             | P.otherwise = ""

-}
