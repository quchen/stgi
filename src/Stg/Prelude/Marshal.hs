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



import qualified Data.Map    as M
import           Data.Maybe
import           Data.Monoid

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

-- FIXME: There's a nil missing on the innermost level when running
--
-- >>> T.putStrLn $ prettyprint $ toStg "arf" ([[[1]]] :: [[[Integer]]])
-- arf = \ => letrec
--                __0_cons = \(__0_value nil) -> Cons __0_value nil;
--                __0_value = \ => letrec
--                                     __0_cons = \(__0_value nil) -> Cons __0_value nil;
--                                     __0_value = \ => letrec
--                                                          __0_cons = \(__0_value nil) -> Cons __0_value nil;
--                                                          __0_value = \ -> Int# 1#
--                                                      in __0_cons;
--                                     nil = \ -> Nil
--                                 in __0_cons;
--                nil = \ -> Nil
--            in __0_cons;
-- nil = \ -> Nil
--
-- This nil is redundant, but it's inconsistent to not have it at the innermost
-- level. (Ideally, we'd like to only have nil bindings at the top level of course.)
--
-- >>>let ppr ast = T.putStrLn (prettyprintPlain ast)
-- >>> ppr (toStg "list" [1, -2, 3 :: Int])
-- list = FIXME
instance ToStg a => ToStg [a] where
    toStg name [] = Stg.nil <> Program (Binds [(name, [stg| \ -> nil |])])
    toStg name dataValues = Stg.nil <>
        Program (Binds [
        ( name
        , LambdaForm [] Update []
            (Let Recursive
                listBindings
                (AppF (mkConsVar 0) []) ))])
      where
        listBindings :: Binds
        listBindings = mkIndexedBinds dataValues (\i value nextIsNil ->

            let valueVar = Var ("__" <> show' i <> "_value")
                Program valueBind = toStg valueVar value

                consVar  = mkConsVar i
                cons'Var | nextIsNil = Var "nil"
                         | otherwise = mkConsVar (i+1)
                consBind = (Binds . M.singleton consVar) (LambdaForm
                    [valueVar, cons'Var]
                    NoUpdate -- Standard constructors are not updatable
                    []
                    (AppC (Constr "Cons")
                          [AtomVar valueVar, AtomVar cons'Var] ))

            in valueBind <> consBind )

        mkIndexedBinds :: ToStg val => [val] -> (Int -> val -> Bool -> Binds) -> Binds
        mkIndexedBinds values mkBinds = mconcat (zipWith3 mkBinds [0..] values nextIsNils)

        nextIsNils :: [Bool]
        nextIsNils = replicate (length dataValues - 1) False <> [True]

        mkConsVar :: Int -> Var
        mkConsVar i = Var ("__" <> show' i <> "_cons")
