{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Convert Haskell values to STG values.
module Stg.Marshal.ToStg (
    ToStg(..),
) where



import           Control.Applicative
import           Control.Monad.Trans.Writer
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Map                   as M
import           Data.Monoid
import           Data.Text                  (Text)

import           Stg.Language
import qualified Stg.Parser.QuasiQuoter as QQ
import qualified Stg.Prelude.List       as Stg
import qualified Stg.Prelude.Maybe      as Stg
import           Stg.Util

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> let ppr = Data.Text.IO.putStrLn . Stg.Language.Prettyprint.prettyprintPlain



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
-- maybe = \ =>
--     let __justVal = \ -> Int# 1#
--     in Just __justVal
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
-- either = \ =>
--     let __leftval = \ -> Int# 1#
--     in Left __leftval
--
-- >>> ppr (toStg "either" (Right 2 :: Either [Int] Int))
-- either = \ =>
--     let __rightval = \ -> Int# 2#
--     in Right __rightval
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
-- list = \ =>
--     letrec __0_value = \ -> Int# 1#;
--            __1_cons = \(__1_value __2_cons) -> Cons __1_value __2_cons;
--            __1_value = \ -> Int# 2#;
--            __2_cons = \(__2_value) -> Cons __2_value nil;
--            __2_value = \ -> Int# 3#
--     in Cons __0_value __1_cons;
-- nil = \ -> Nil
instance ToStg a => ToStg [a] where
    toStgWithGlobals name dataValues = do
        tell Stg.nil
        case dataValues of
            (x:xs) -> do
                (Just inExpression, letBindings)
                    <- mkListBinds Nothing (NonEmpty.zip [0..] (x :| xs))
                pure (Program (Binds [(name, LambdaForm [] Update []
                    (Let Recursive letBindings inExpression) )]))
            _nil -> pure (Program (Binds [(name, [QQ.stg| \ -> nil |])]))
      where

        mkConsVar :: Int -> Var
        mkConsVar i = Var (genPrefix <> show' i <> "_cons")

        mkListBinds
            :: ToStg value
            => Maybe Expr -- ^ Has the 'in' part of the @let@ already been
                          -- set, and if yes to what? Used to avoid allocating
                          -- the first cons cell, avoiding an immediate GC.
            -> NonEmpty (Int, value) -- ^ Index and value of the cells
            -> Writer Program (Maybe Expr, Binds)
        mkListBinds inExpression ((i, value) :| rest) = do

            let valueVar = Var (genPrefix <> show' i <> "_value")
            Program valueBind <- toStgWithGlobals valueVar value

            (inExpression', restBinds) <- do
                let consVar = mkConsVar i
                    nextConsVar = if null rest then Var "nil"
                                               else mkConsVar (i+1)
                    consBind = case inExpression of
                        Nothing -> mempty
                        Just _ -> (Binds . M.singleton consVar) (LambdaForm
                            (valueVar : [nextConsVar | not (null rest)])
                            NoUpdate -- Standard constructors are not updatable
                            []
                            consExpr )
                    consExpr = AppC (Constr "Cons") (map AtomVar [valueVar, nextConsVar])

                    inExpression' = inExpression <|> Just consExpr

                recursiveBinds <- case rest of
                    (i',v') : isvs -> fmap snd (mkListBinds inExpression' ((i',v') :| isvs))
                    _nil           -> pure mempty

                pure (inExpression', consBind <> recursiveBinds)

            pure (inExpression', valueBind <> restBinds)

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
-- pair = \ =>
--     let __fst = \ -> Int# 1#;
--         __snd = \ -> Int# 2#
--     in Pair __fst __snd
instance (ToStg a, ToStg b) => ToStg (a,b) where
    toStgWithGlobals name (x,y) = do
        (fstBindName, fstBind) <- tupleEntry "fst" x
        (sndBindName, sndBind) <- tupleEntry "snd" y
        let allBinds = fstBind <> sndBind
            allBindNames = [fstBindName, sndBindName]
        pure (Program (tupleBinds name (Constr "Pair") allBinds allBindNames))

-- | >>> ppr (toStg "triple" (1::Int, 2::Int, 3::Int))
-- triple = \ =>
--     let __fst3 = \ -> Int# 1#;
--         __snd3 = \ -> Int# 2#;
--         __trd3 = \ -> Int# 3#
--     in Triple __fst3 __snd3 __trd3
instance (ToStg a, ToStg b, ToStg c) => ToStg (a,b,c) where
    toStgWithGlobals name (x,y,z) = do
        (fstBindName, fstBind) <- tupleEntry "fst3" x
        (sndBindName, sndBind) <- tupleEntry "snd3" y
        (trdBindName, trdBind) <- tupleEntry "trd3" z
        let allBinds = fstBind <> sndBind <> trdBind
            allBindNames = [fstBindName, sndBindName, trdBindName]
        pure (Program (tupleBinds name (Constr "Triple") allBinds allBindNames))

-- | >>> ppr (toStg "quadruple" (1::Int, 2::Int, 3::Int, 4::Int))
-- quadruple = \ =>
--     let __fou4 = \ -> Int# 4#;
--         __fst4 = \ -> Int# 1#;
--         __snd4 = \ -> Int# 2#;
--         __trd4 = \ -> Int# 3#
--     in Quadruple __fst4 __snd4 __trd4 __fou4
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
-- quintuple = \ =>
--     let __fif5 = \ -> Int# 5#;
--         __fou5 = \ -> Int# 4#;
--         __fst5 = \ -> Int# 1#;
--         __snd5 = \ -> Int# 2#;
--         __trd5 = \ -> Int# 3#
--     in Quintuple __fst5 __snd5 __trd5 __fou5 __fif5
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
