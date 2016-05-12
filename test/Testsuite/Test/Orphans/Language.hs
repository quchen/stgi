{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Orphans.Language () where



import qualified Data.List.NonEmpty     as NonEmpty
import qualified Data.Map               as M
import           Data.Monoid            hiding (Alt)
import           Data.Ratio
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Test.SmallCheck.Series
import           Test.Tasty.QuickCheck

import Stg.Language

import Test.Util



--------------------------------------------------------------------------------
-- Helper functions

lowerChar :: Gen Text
lowerChar = resize 3 (T.singleton <$> elements ['a'..'z'])

upperChar :: Gen Text
upperChar = resize 3 (T.singleton <$> elements ['A'..'Z'])

extendedLetters :: Gen Text
extendedLetters = resize 3 (T.pack <$> listOf extendedLetter)
  where
    extendedLetter :: Gen Char
    extendedLetter = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "\'_")

shrinkBut1stLetter :: Text -> [Text]
shrinkBut1stLetter text = case T.uncons text of
    Nothing -> []
    Just (x, xs) ->
        let shrunkenRest = map T.pack . shrink . T.unpack
        in map (T.singleton x <>) (shrunkenRest xs)

-- | Shrink all but the first element. Useful for e.g. bindings, because there
-- has to be at least one of them, so we don't want to shrink them all away.
shrinkBut1st :: Arbitrary a => [a] -> [[a]]
shrinkBut1st [] = []
shrinkBut1st (x:xs) = map (x:) (shrink xs)

reservedKeywords :: [Var]
reservedKeywords = ["let", "in", "case", "of"]


--------------------------------------------------------------------------------
-- QuickCheck

instance Arbitrary Program where
    arbitrary = arbitrary1 Program
    shrink = genericShrink

instance Arbitrary Binds where
    arbitrary = do
        xs <- listOf1 (scaled (2%3) arbitrary)
        pure (Binds (M.fromList xs))
    shrink (Binds b) = (map (Binds . M.fromList) . shrinkBut1st . M.toList) b

instance Arbitrary LambdaForm where
    arbitrary = do
        free <- arbitrary
        updateFlag <- arbitrary
        bound <- case updateFlag of
            Update -> pure []
            NoUpdate -> arbitrary
        body <- oneof
            ([ arbitrary3 Let
            , arbitrary2 Case
            -- Lambdas cannot have primitive type, so we exclude AppP and Lit
            , arbitrary2 AppF ]
            <>
            -- Standard constructors are never updatable, so we exclude those
            [arbitrary2 AppC | updateFlag == NoUpdate] )
        pure (LambdaForm free updateFlag bound body)
    shrink = genericShrink

instance Arbitrary UpdateFlag where
    arbitrary = allEnums
    shrink = genericShrink

instance Arbitrary Rec where
    arbitrary = allEnums
    shrink = genericShrink

instance Arbitrary Expr where
    arbitrary = oneof
        [ arbitrary3 Let
        , arbitrary2 Case
        , arbitrary2 AppF
        , arbitrary2 AppC
        , arbitrary3 AppP
        , arbitrary1 Lit ]
    shrink = genericShrink

instance Arbitrary Alts where
    arbitrary = arbitrary2 Alts

instance Arbitrary NonDefaultAlts where
    arbitrary = oneof
        [ pure NoNonDefaultAlts
        , fmap (AlgebraicAlts . NonEmpty.fromList)
            (listOf1 (scaled (2%3) (arbitrary3 AlgebraicAlt)))
        , fmap (PrimitiveAlts . NonEmpty.fromList)
            (listOf1 (scaled (2%3) (arbitrary2 PrimitiveAlt))) ]

instance Arbitrary DefaultAlt where
    arbitrary = oneof [arbitrary1 DefaultNotBound, arbitrary2 DefaultBound]
    shrink = genericShrink

instance Arbitrary Literal where
    arbitrary = fmap Literal (resize 128 arbitrary)
    shrink = genericShrink

instance Arbitrary PrimOp where
    arbitrary = allEnums
    shrink = genericShrink

instance Arbitrary Var where
    arbitrary = do
        x <- lowerChar
        xs <- extendedLetters
        let var = Var (x <> xs)
        if var `elem` reservedKeywords
            then arbitrary
            else pure var
    shrink (Var var) = map Var (shrinkBut1stLetter var)

instance Arbitrary Atom where
    arbitrary = oneof [arbitrary1 AtomVar, arbitrary1 AtomLit]
    shrink = genericShrink

instance Arbitrary Constr where
    arbitrary = do
        x <- upperChar
        xs <- extendedLetters
        (pure . Constr) (x <> xs)
    shrink (Constr constr) = map Constr (shrinkBut1stLetter constr)


--------------------------------------------------------------------------------
-- SmallCheck

instance Monad m => Serial m Literal
instance Monad m => Serial m PrimOp
instance Monad m => Serial m Rec
instance Monad m => Serial m UpdateFlag
