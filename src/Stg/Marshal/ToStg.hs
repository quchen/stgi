{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Convert Haskell values to STG values.
module Stg.Marshal.ToStg (
    ToStg(..),
) where



import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Map                   as M
import           Data.Text                  (Text)

import           Stg.Language
import qualified Stg.Parser.QuasiQuoter as QQ
import qualified Stg.Prelude.List       as Stg
import qualified Stg.Prelude.Maybe      as Stg
import           Stg.Util

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Stg.Language.Prettyprint
-- >>> let ppr = Data.Text.IO.putStrLn . Stg.Language.Prettyprint.renderPlain . prettyStgi



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
-- maybe = \ => nothing;
-- nothing = \ -> Nothing
--
-- >>> ppr (toStg "maybe" (Just 1 :: Maybe Int))
-- maybe = \ =>
--     let __justVal = \ -> Int# 1#
--     in Just __justVal
instance ToStg a => ToStg (Maybe a) where
    toStgWithGlobals name Nothing = do
        tell Stg.nothing
        pure (Program (Binds [(name, [QQ.stg| \ => nothing |])]))
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
-- list = \ => nil;
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
            x:xs -> do
                (inExpression, letBindings)
                    <- mkListBinds Nothing (NonEmpty.zip [0..] (x :| xs))
                let rec = if null xs then NonRecursive else Recursive
                pure (Program (Binds [(name, LambdaForm [] Update []
                    (Let rec letBindings inExpression) )]))
            _nil -> pure (Program (Binds [(name, [QQ.stg| \ => nil |])]))
      where

        mkConsVar :: Int -> Var
        mkConsVar i = Var (genPrefix <> show' i <> "_cons")

        mkListBinds
            :: ToStg value
            => Maybe Expr -- ^ Has the 'in' part of the @let@ already been
                          -- set, and if yes to what? Used to avoid allocating
                          -- the first cons cell, avoiding an immediate GC.
            -> NonEmpty (Int, value) -- ^ Index and value of the cells
            -> Writer Program (Expr, Binds)
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

                    Just inExpression' = inExpression <|> Just consExpr

                recursiveBinds <- case rest of
                    (i',v') : isvs -> fmap snd (mkListBinds (Just inExpression') ((i',v') :| isvs))
                    _nil           -> pure mempty

                pure (inExpression', consBind <> recursiveBinds)

            pure (inExpression', valueBind <> restBinds)

tupleEntry
    :: ToStg value
    => Text
    -> value
    -> WriterT Binds (Writer Program) ()
tupleEntry name val = do
    let bindName = Var (genPrefix <> name)
    Program bind <- lift (toStgWithGlobals bindName val)
    tell bind

-- | This definition unifies the creation of tuple bindings to reduce code
-- duplication between the tuple instances.
tupleBinds
    :: Var    -- ^ Name of the tuple binding
    -> Constr -- ^ Name of the tuple constructor, e.g. \"Pair"
    -> Binds  -- ^ Bindings of the entries
    -> Binds
tupleBinds name tupleCon binds  =
    let bindVars =
            let Binds b = binds
            in M.keys b
    in Binds [(name,
        LambdaForm [] Update []
            (Let NonRecursive
                binds
                (AppC tupleCon (map AtomVar bindVars)) ))]

-- | >>> ppr (toStg "pair" ((1,2) :: (Int,Int)))
-- pair = \ =>
--     let __fst = \ -> Int# 1#;
--         __snd = \ -> Int# 2#
--     in Pair __fst __snd
instance (ToStg a, ToStg b) => ToStg (a,b) where
    toStgWithGlobals name (x,y) = do
        binds <- execWriterT (do
            tupleEntry "fst" x
            tupleEntry "snd" y )
        pure (Program (tupleBinds name (Constr "Pair") binds))

-- | >>> ppr (toStg "triple" ((1,2,3) :: (Int,Int,Int)))
-- triple = \ =>
--     let __x = \ -> Int# 1#;
--         __y = \ -> Int# 2#;
--         __z = \ -> Int# 3#
--     in Triple __x __y __z
instance (ToStg a, ToStg b, ToStg c) => ToStg (a,b,c) where
    toStgWithGlobals name (x,y,z) = do
        binds <- execWriterT (do
            tupleEntry "x" x
            tupleEntry "y" y
            tupleEntry "z" z )
        pure (Program (tupleBinds name (Constr "Triple") binds))

-- | >>> ppr (toStg "quadruple" ((1,2,3,4) :: (Int,Int,Int,Int)))
-- quadruple = \ =>
--     let __w = \ -> Int# 1#;
--         __x = \ -> Int# 2#;
--         __y = \ -> Int# 3#;
--         __z = \ -> Int# 4#
--     in Quadruple __w __x __y __z
instance (ToStg a, ToStg b, ToStg c, ToStg d) => ToStg (a,b,c,d) where
    toStgWithGlobals name (w4,x4,y4,z4) = do
        binds <- execWriterT (do
            tupleEntry "w" w4
            tupleEntry "x" x4
            tupleEntry "y" y4
            tupleEntry "z" z4 )
        pure (Program (tupleBinds name (Constr "Quadruple") binds))

-- | >>> ppr (toStg "quintuple" ((1,2,3,4,5) :: (Int,Int,Int,Int,Int)))
-- quintuple = \ =>
--     let __v = \ -> Int# 1#;
--         __w = \ -> Int# 2#;
--         __x = \ -> Int# 3#;
--         __y = \ -> Int# 4#;
--         __z = \ -> Int# 5#
--     in Quintuple __v __w __x __y __z
instance (ToStg a, ToStg b, ToStg c, ToStg d, ToStg e) => ToStg (a,b,c,d,e) where
    toStgWithGlobals name (v5,w5,x5,y5,z5) = do
        binds <- execWriterT (do
            tupleEntry "v" v5
            tupleEntry "w" w5
            tupleEntry "x" x5
            tupleEntry "y" y5
            tupleEntry "z" z5 )
        pure (Program (tupleBinds name (Constr "Quintuple") binds))
