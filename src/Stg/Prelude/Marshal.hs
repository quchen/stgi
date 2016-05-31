{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Convert Haskell values to STG values and back.
module Stg.Prelude.Marshal (
    ToStg(toStg),
    FromStg(..),
    FromStgError(..),
) where



import           Control.Applicative
import           Control.Monad.Trans.Writer
import qualified Data.Map                   as M
import           Data.Monoid
import           Data.Text                  (Text)

import           Stg.Language
import qualified Stg.Machine.Env        as Env
import qualified Stg.Machine.Heap       as H
import           Stg.Machine.Types
import qualified Stg.Parser.QuasiQuoter as QQ
import qualified Stg.Prelude.List       as Stg
import qualified Stg.Prelude.Maybe      as Stg
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

    -- | Retrieve the value of a global variable.
    fromStg
        :: StgState
        -> Var -- ^ Name of the global, e.g. @main@
        -> Either FromStgError value
    fromStg stgState = globalVal stgState (\case
        PrimInt{} -> Left TypeMismatch
        Addr addr -> fromStgAddr stgState addr )

    -- | Retrieve the value of a heap address.
    fromStgAddr
        :: StgState
        -> MemAddr
        -> Either FromStgError value

    -- | Used only for looking up primitive integers.
    fromStgPrim
        :: Integer
        -> Either FromStgError value
    fromStgPrim _ = Left TypeMismatch

    {-# MINIMAL fromStgAddr #-}

data FromStgError =
      TypeMismatch        -- ^ e.g. asking for an @Int#@ at an address
                          --   that contains a @Cons@
    | NotInNormalForm     -- ^ Tried retrieving a thunk
    | IsBlackhole         -- ^ Tried retrieving a thunk
    | BadConstructor      -- ^ e.g. @Cons x y z@
    | NotFound NotInScope -- ^ A variable lookup unsuccessful
    | AddrNotOnHeap

-- | Look up the global of a variable and handle the result.
--
-- Slighly bent version of 'Env.globalVal' to fit the types in this module
-- better.
globalVal
    :: FromStg value
    => StgState
    -> (Value -> Either FromStgError value) -- ^ What to do with the value if found
    -> Var                                  -- ^ Name of the global value to inspect
    -> Either FromStgError value
globalVal stgState f var = case Env.globalVal (stgGlobals stgState) (AtomVar var) of
    Failure _ -> Left (NotFound (NotInScope [var]))
    Success v -> f v

-- | Create a local environment.
--
-- Slighly bent version of 'Env.makeLocals' to fit the types in this module
-- better.
makeLocals :: [Var] -> [Value] -> Locals
makeLocals freeVars freeVals = Env.makeLocals (zipWith Mapping freeVars freeVals)

-- | Look up the value of an 'Atom' in a state, given a local environment.
atomVal
    :: FromStg value
    => StgState
    -> Locals
    -> Atom
    -> Either FromStgError value
atomVal stgState locals var = case Env.val locals (stgGlobals stgState) var of
    Failure notInScope -> Left (NotFound notInScope)
    Success (Addr addr) -> fromStgAddr stgState addr
    Success (PrimInt i)  -> fromStgPrim i

-- | Inspect whether a closure at a certain memory address matches the desired
-- criteria.
inspect
    :: FromStg value
    => StgState
    -> (Closure -> Either FromStgError value)
    -> MemAddr
    -> Either FromStgError value
inspect stgState inspectClosure addr = case H.lookup addr (stgHeap stgState) of
    Nothing -> Left AddrNotOnHeap
    Just heapObject -> case heapObject of
        Blackhole{} -> Left IsBlackhole
        HClosure closure -> inspectClosure closure

instance FromStg () where
    fromStgAddr stgState = inspect stgState (\case
        Closure (LambdaForm _ _ args _) _
            | not (null args) -> Left BadConstructor
        Closure (LambdaForm _ _ _ (AppC "Unit" [])) _
            -> pure ()
        Closure (LambdaForm _ _ _ (AppC "Unit" _)) _
            -> Left BadConstructor
        Closure _ _
            -> Left TypeMismatch )

instance FromStg Integer where
    fromStg stgState = globalVal stgState (\case
        PrimInt i -> Right i
        Addr addr -> fromStgAddr stgState addr )

    fromStgAddr stgState addr = case H.lookup addr (stgHeap stgState) of
        Nothing -> Left AddrNotOnHeap
        Just heapObject -> case heapObject of
            Blackhole{} -> Left IsBlackhole
            HClosure closure -> inspectClosure closure
      where
        inspectClosure = \case
            Closure (LambdaForm _ _ args _) _
                | not (null args) -> Left BadConstructor
            Closure (LambdaForm _ _ _ (AppC "Int#" intArgs)) _
                | not (intArgs `lengthEquals` 1) -> Left BadConstructor
            Closure (LambdaForm freeVars _ _ (AppC "Int#" [arg])) freeVals
                -> let locals = makeLocals freeVars freeVals
                   in atomVal stgState locals arg
            Closure _ _
                -> Left TypeMismatch

    fromStgPrim i = Right i

instance (FromStg a, FromStg b) => FromStg (a,b) where
    fromStgAddr stgState = inspect stgState (\case
        Closure (LambdaForm _ _ args _) _
            | not (null args) -> Left BadConstructor
        Closure (LambdaForm _ _ _ (AppC "Pair" args)) _
            | not (args `lengthEquals` 2) -> Left BadConstructor
        Closure (LambdaForm freeVars _ _ (AppC "Pair" [x, y])) freeVals
            -> let locals = makeLocals freeVars freeVals
               in liftA2 (,) (atomVal stgState locals x)
                             (atomVal stgState locals y)
        Closure _ _
            -> Left TypeMismatch )

instance (FromStg a, FromStg b) => FromStg (Either a b) where
    fromStgAddr stgState = inspect stgState (\case
        Closure (LambdaForm _ _ args _) _
            | not (null args) -> Left BadConstructor
        Closure (LambdaForm _ _ _ (AppC con args)) _
            | not (args `lengthEquals` 2 && (con == "Left" || con == "Right")) -> Left BadConstructor
        Closure (LambdaForm freeVars _ _ (AppC "Left" [l])) freeVals
            -> let locals = makeLocals freeVars freeVals
               in fmap Left (atomVal stgState locals l)
        Closure (LambdaForm freeVars _ _ (AppC "Right" [r])) freeVals
            -> let locals = makeLocals freeVars freeVals
               in fmap Right (atomVal stgState locals r)
        Closure _ _
            -> Left TypeMismatch )

lengthEquals :: [a] -> Int -> Bool
lengthEquals [] 0     = True
lengthEquals (_:xs) n = lengthEquals xs $! n-1
lengthEquals _ _      = False



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

-- | >>> ppr (toStg "unit" ())
-- unit = \ -> Unit
instance ToStg () where
    toStg name _ = Program (Binds [(name, LambdaForm [] NoUpdate []
        (AppC (Constr "Unit") []) )])

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

-- | >>> ppr (toStg "maybe" (Nothing :: Maybe Int))
-- maybe = \ -> nothing;
-- nothing = \ -> Nothing
--
-- >>> ppr (toStg "maybe" (Just 1 :: Maybe Int))
-- maybe = \ => let __justVal = \ -> Int# 1#
--              in Just __justVal
instance ToStg a => ToStg (Maybe a) where
    toStgWithGlobals name Nothing = do
        tell Stg.nothing
        pure (Program (Binds [(name, [QQ.stg| \ -> nothing |])]))
    toStgWithGlobals name (Just x) = do
        Program xBinding <- toStgWithGlobals justBindName x
        pure (Program (Binds [
            ( name
            , LambdaForm [] Update []
                (Let NonRecursive
                    xBinding
                    (AppC "Just" [AtomVar justBindName]) ))]))
      where
        justBindName :: Var
        justBindName = Var (genPrefix <> "justVal")

-- | >>> ppr (toStg "either" (Left 1 :: Either Int [Int]))
-- either = \ => let __leftval = \ -> Int# 1#
--               in Left __leftval
--
-- >>> ppr (toStg "either" (Right 2 :: Either [Int] Int))
-- either = \ => let __rightval = \ -> Int# 2#
--               in Right __rightval
instance (ToStg a, ToStg b) => ToStg (Either a b) where
    toStgWithGlobals name x = do
        let bindName = Var (genPrefix <> chooseEither "left" "right" x <> "val")
        Program xBinding <- case x of
            Left l  -> toStgWithGlobals bindName l
            Right r -> toStgWithGlobals bindName r
        pure (Program (Binds [
            ( name
            , LambdaForm [] Update []
                (Let NonRecursive
                    xBinding
                    (AppC (chooseEither "Left" "Right" x) [AtomVar bindName]) ))]))
          where
            chooseEither l _ (Left  _) = l
            chooseEither _ r (Right _) = r

-- | >>> ppr (toStg "list" ([] :: [Int]))
-- list = \ -> nil;
-- nil = \ -> Nil
--
-- >>> ppr (toStg "list" [1, 2, 3 :: Int])
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
