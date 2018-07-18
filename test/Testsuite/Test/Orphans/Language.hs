{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Orphans.Language () where



import           Control.Applicative
import qualified Data.List.NonEmpty  as NonEmpty
import qualified Data.Map            as M
import           Data.Ratio
import           Data.Text           (Text)
import qualified Data.Text           as T

import Stg.Language
import Stg.Parser.QuasiQuoter

import Test.SmallCheck.Series
import Test.Tasty.QuickCheck
import Test.Util



--------------------------------------------------------------------------------
-- Helpers

shrinkBut1stLetter :: Text -> [Text]
shrinkBut1stLetter text = case T.uncons text of
    Nothing -> []
    Just (x, xs) ->
        let shrunkenRest = map T.pack . shrink . T.unpack
        in map (T.singleton x <>) (shrunkenRest xs)

reservedKeywords :: [Var]
reservedKeywords = ["let", "in", "case", "of", "default"]


--------------------------------------------------------------------------------
-- QuickCheck

instance Arbitrary Program where
    arbitrary = arbitrary_1 Program
    shrink = genericShrink

instance Arbitrary Binds where
    arbitrary = do
        xs <- listOf1 (scaled (2%3) arbitrary)
        pure (Binds (M.fromList xs))
    shrink (Binds b) =
          [binds| bind = \ -> Unit |]
        : [binds| bind1 = \ -> Unit1; bind2 = \ -> Unit2 |]
        : (map (Binds . M.fromList) . shrinkBut1st . M.toList) b
      where
        -- Bindings have to be non-empty, we ensure at least one element is in
        -- the shrunken result.
        shrinkBut1st [] = []
        shrinkBut1st (x:xs) = liftA2 (:) (shrink x) (shrink xs)

instance Arbitrary LambdaForm where
    arbitrary = do
        free <- arbitrary
        updateFlag <- arbitrary
        bound <- case updateFlag of
            Update -> pure []
            NoUpdate -> arbitrary
        body <- oneof
            ([ arbitrary_3 Let
            , arbitrary_2 Case
            -- Lambdas cannot have primitive type, so we exclude AppP and Lit
            , arbitrary_2 AppF ]
            <>
            -- Standard constructors are never updatable, so we exclude those
            [arbitrary_2 AppC | updateFlag == NoUpdate] )
        pure (LambdaForm free updateFlag bound body)
    shrink lf =
          [lambdaForm| \ -> x |]
        : [lambdaForm| \ => x |]
        : [lambdaForm| \x -> x |]
        : [lambdaForm| \(y) x -> x |]
        : [lambdaForm| \(y z) x w -> Con x y z w |]
        : filter isValid (genericShrink lf)
      where
        isValid = \case
            LambdaForm _ Update (_:_) AppF{} -> False
            LambdaForm _ Update (_:_) AppC{} -> False
            LambdaForm _ _      _     AppP{} -> False
            LambdaForm _ _      _     LitE{} -> False
            _other -> True

instance Arbitrary UpdateFlag where
    arbitrary = allEnums
    shrink = genericShrink

instance Arbitrary Rec where
    arbitrary = allEnums
    shrink = genericShrink

instance Arbitrary Expr where
    arbitrary = oneof
        [ arbitrary_3 Let
        , arbitrary_2 Case
        , arbitrary_2 AppF
        , arbitrary_2 AppC
        , arbitrary_3 AppP
        , arbitrary_1 LitE ]
    shrink = genericShrink

instance Arbitrary Alts where
    arbitrary = arbitrary_2 Alts

instance Arbitrary NonDefaultAlts where
    arbitrary = oneof
        [ pure NoNonDefaultAlts
        , fmap (AlgebraicAlts . NonEmpty.fromList)
            (listOf1 (scaled (2%3) (arbitrary_3 AlgebraicAlt)))
        , fmap (PrimitiveAlts . NonEmpty.fromList)
            (listOf1 (scaled (2%3) (arbitrary_2 PrimitiveAlt))) ]

instance Arbitrary DefaultAlt where
    arbitrary = oneof [arbitrary_1 DefaultNotBound, arbitrary_2 DefaultBound]
    shrink = genericShrink

instance Arbitrary Literal where
    arbitrary = resize 128 (arbitrary_1 Literal)
    shrink = genericShrink

instance Arbitrary PrimOp where
    arbitrary = allEnums
    shrink = genericShrink

word, lower, upper :: Gen Text
word = T.pack <$> listOf (elements (['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "\'_"))
lower = T.singleton <$> elements ['a'..'z']
upper = T.singleton <$> elements ['A'..'Z']

instance Arbitrary Var where
    arbitrary = do
        var <- (\head' tail' -> Var (head' <> tail')) <$> lower <*> word
        if var `elem` reservedKeywords
            then arbitrary
            else pure var
    shrink (Var var) = map Var (shrinkBut1stLetter var)

instance Arbitrary Atom where
    arbitrary = oneof [arbitrary_1 AtomVar, arbitrary_1 AtomLit]
    shrink = genericShrink

instance Arbitrary Constr where
    arbitrary = (\head' tail' hash -> Constr (head' <> tail' <> hash))
                <$> upper
                <*> word
                <*> frequency [(3, pure ""), (1, pure "#")]
    shrink (Constr constr) = map Constr (shrinkBut1stLetter constr)


--------------------------------------------------------------------------------
-- SmallCheck

instance Monad m => Serial m Literal
instance Monad m => Serial m PrimOp
instance Monad m => Serial m Rec
instance Monad m => Serial m UpdateFlag
