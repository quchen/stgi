{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Convert Haskell values to STG values and back.
module Stg.Prelude.Marshal (
    ToStg(toStg),
    FromStg(..),
) where



import           Control.Monad.Trans.Writer
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text)

import           Stg.Language
import           Stg.Machine.Env        (globalVal)
import qualified Stg.Machine.Heap       as H
import           Stg.Machine.Types
import qualified Stg.Parser.QuasiQuoter as QQ
import qualified Stg.Prelude.List       as Stg
import           Stg.Util

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> let ppr = Data.Text.IO.putStrLn . Stg.Language.Prettyprint.prettyprintPlain



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



-- | Prefix for all generated variables
genPrefix :: Text
genPrefix = "__"

-- | Convert a Haskell value to an STG binding.
--
-- Instances of this class should have a corresponding 'FromStg' instance to
-- retrieve a value fom the program, with the two being inverse to each other
-- (up to forcing the generated thunks).
--
-- This class contains a helper function, 'toStgWithGlobals', this is hidden
-- from the outside. If you want to write your own instance, have a look at the
-- source for documentation.
class ToStg value where
    toStg
        :: Var -- ^ Name of the binding
        -> value
        -> Program
    toStg var val =
        let (globals, actualDef) = runWriter (toStgWithGlobals var val)
        in globals <> actualDef

    -- | Some definitions, such as the one for lists, require certain global
    -- values to be present (such as nil). In order to avoid duplicate
    -- definitions, this function allows defining top-level elements using
    -- 'Writer's 'tell' function.
    toStgWithGlobals
        :: Var -- ^ Name of the binding
        -> value
        -> Writer Program Program -- ^ Log: globals; value: value definition itself
    toStgWithGlobals var val = pure (toStg var val)

    {-# MINIMAL toStg | toStgWithGlobals #-}

-- | >>> ppr (toStg "int" (1 :: Integer))
-- int = \ -> Int# 1#
instance ToStg Integer where
    toStg name i = Program (Binds [(name, LambdaForm [] NoUpdate []
        (AppC (Constr "Int#") [AtomLit (Literal i)]) )])

-- | Same as the 'Integer' instance, but makes for shorter type annotations
instance ToStg Int where
    toStg name i = toStg name (fromIntegral i :: Integer)

-- | >>> ppr (toStg "bool" True)
-- bool = \ -> True
instance ToStg Bool where
    toStg name b = Program (Binds [(name, LambdaForm [] NoUpdate []
        (AppC (Constr (show' b)) []) )])

-- | >>> ppr (toStg "list" [1, 2, 3 :: Int])
-- list = \ => letrec
--                 __0_cons = \(__0_value __1_cons) -> Cons __0_value __1_cons;
--                 __0_value = \ -> Int# 1#;
--                 __1_cons = \(__1_value __2_cons) -> Cons __1_value __2_cons;
--                 __1_value = \ -> Int# 2#;
--                 __2_cons = \(__2_value) -> Cons __2_value nil;
--                 __2_value = \ -> Int# 3#
--             in __0_cons;
-- nil = \ -> Nil
instance ToStg a => ToStg [a] where
    toStgWithGlobals name dataValues = do
        tell Stg.nil
        if null dataValues
            then pure (Program (Binds [(name, [QQ.stg| \ -> nil |])]))
            else do
                letBindings <- listBindings
                pure (Program (Binds [
                    ( name
                    , LambdaForm [] Update []
                        (Let Recursive
                            letBindings
                            (AppF (mkConsVar 0) []) ))]))
      where
        listBindings :: Writer Program Binds
        listBindings = mkIndexedBinds dataValues (\i value nextIsNil -> do

            let valueVar = Var (genPrefix <> show' i <> "_value")
            Program valueBind <- toStgWithGlobals valueVar value

            let consVar  = mkConsVar i
                cons'Var | nextIsNil = Var "nil"
                         | otherwise = mkConsVar (i+1)
                consBind = (Binds . M.singleton consVar) (LambdaForm
                    (valueVar : if nextIsNil then [] else [cons'Var])
                    NoUpdate -- Standard constructors are not updatable
                    []
                    (AppC (Constr "Cons")
                          [AtomVar valueVar, AtomVar cons'Var] ))

            pure (valueBind <> consBind) )

        mkIndexedBinds
            :: ToStg val
            => [val]
            -> (Int -> val -> Bool -> Writer Program Binds)
            -> Writer Program Binds
        mkIndexedBinds values mkBinds = fmap mconcat (sequence (zipWith3 mkBinds [0..] values nextIsNils))

        nextIsNils :: [Bool]
        nextIsNils = replicate (length dataValues - 1) False <> [True]

        mkConsVar :: Int -> Var
        mkConsVar i = Var (genPrefix <> show' i <> "_cons")

tupleEntry :: ToStg value => Text -> value -> Writer Program (Var, Binds)
tupleEntry name val = do
    let bindName = Var (genPrefix <> name)
    Program bind <- toStgWithGlobals bindName val
    pure (bindName, bind)

-- | This definition unifies the creation of tuple bindings to reduce code
-- duplication between the tuple instances.
tupleBinds
    :: Var    -- ^ Name of the tuple binding
    -> Constr -- ^ Name of the tuple constructor, e.g. \"Pair"
    -> Binds  -- ^ Bindings of the entries
    -> [Var]  -- ^ Names of the bindings of the entries
    -> Binds
tupleBinds name tupleCon binds entryBindVars =
    Binds [(name,
        LambdaForm [] Update []
            (Let NonRecursive
                binds
                (AppC tupleCon (map AtomVar entryBindVars)) ))]

-- | >>> ppr (toStg "pair" (1::Int, 2::Int))
-- pair = \ => let __fst = \ -> Int# 1#;
--                 __snd = \ -> Int# 2#
--             in Pair __fst __snd
instance (ToStg a, ToStg b) => ToStg (a,b) where
    toStgWithGlobals name (x,y) = do
        (fstBindName, fstBind) <- tupleEntry "fst" x
        (sndBindName, sndBind) <- tupleEntry "snd" y
        let allBinds = fstBind <> sndBind
            allBindNames = [fstBindName, sndBindName]
        pure (Program (tupleBinds name (Constr "Pair") allBinds allBindNames))

-- | >>> ppr (toStg "triple" (1::Int, 2::Int, 3::Int))
-- triple = \ => let __fst3 = \ -> Int# 1#;
--                   __snd3 = \ -> Int# 2#;
--                   __trd3 = \ -> Int# 3#
--               in Triple __fst3 __snd3 __trd3
instance (ToStg a, ToStg b, ToStg c) => ToStg (a,b,c) where
    toStgWithGlobals name (x,y,z) = do
        (fstBindName, fstBind) <- tupleEntry "fst3" x
        (sndBindName, sndBind) <- tupleEntry "snd3" y
        (trdBindName, trdBind) <- tupleEntry "trd3" z
        let allBinds = fstBind <> sndBind <> trdBind
            allBindNames = [fstBindName, sndBindName, trdBindName]
        pure (Program (tupleBinds name (Constr "Triple") allBinds allBindNames))

-- | >>> ppr (toStg "quadruple" (1::Int, 2::Int, 3::Int, 4::Int))
-- quadruple = \ => let __fou4 = \ -> Int# 4#;
--                      __fst4 = \ -> Int# 1#;
--                      __snd4 = \ -> Int# 2#;
--                      __trd4 = \ -> Int# 3#
--                  in Quadruple __fst4 __snd4 __trd4 __fou4
instance (ToStg a, ToStg b, ToStg c, ToStg d) => ToStg (a,b,c,d) where
    toStgWithGlobals name (x,y,z,w) = do
        (fstBindName, fstBind) <- tupleEntry "fst4" x
        (sndBindName, sndBind) <- tupleEntry "snd4" y
        (trdBindName, trdBind) <- tupleEntry "trd4" z
        (fouBindName, fouBind) <- tupleEntry "fou4" w
        let allBinds = fstBind <> sndBind <> trdBind <> fouBind
            allBindNames = [fstBindName, sndBindName, trdBindName, fouBindName]
        pure (Program (tupleBinds name (Constr "Quadruple") allBinds allBindNames))

-- | >>> ppr (toStg "quintuple" (1::Int, 2::Int, 3::Int, 4::Int, 5::Int))
-- quintuple = \ => let __fif5 = \ -> Int# 5#;
--                      __fou5 = \ -> Int# 4#;
--                      __fst5 = \ -> Int# 1#;
--                      __snd5 = \ -> Int# 2#;
--                      __trd5 = \ -> Int# 3#
--                  in Quintuple __fst5 __snd5 __trd5 __fou5 __fif5
instance (ToStg a, ToStg b, ToStg c, ToStg d, ToStg e) => ToStg (a,b,c,d,e) where
    toStgWithGlobals name (x,y,z,w,v) = do
        (fstBindName, fstBind) <- tupleEntry "fst5" x
        (sndBindName, sndBind) <- tupleEntry "snd5" y
        (trdBindName, trdBind) <- tupleEntry "trd5" z
        (fouBindName, fouBind) <- tupleEntry "fou5" w
        (fifBindName, fifBind) <- tupleEntry "fif5" v
        let allBinds = fstBind <> sndBind <> trdBind <> fouBind <> fifBind
            allBindNames = [fstBindName, sndBindName, trdBindName, fouBindName, fifBindName]
        pure (Program (tupleBinds name (Constr "Quintuple") allBinds allBindNames))
