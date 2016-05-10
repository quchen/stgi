{-# LANGUAGE OverloadedStrings #-}

-- | Useful utilities that don't really fit in a specific location.
module Stg.Util (
    show',
    Validate(..),

    -- * Prettyprinter extensions
    commaSep,
    tupled',
    bulletList,
    pluralS,
) where



import           Data.Bifunctor
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))



-- | 'show' with 'Text' as codomain.
--
-- @
-- show' = 'T.pack' . 'show'
-- @
show' :: Show a => a -> Text
show' = T.pack . show



-- | The validation version of 'Either'.
data Validate err a = Failure err | Success a

instance Functor (Validate a) where
    fmap _ (Failure err) = Failure err
    fmap f (Success x)   = Success (f x)

instance Bifunctor Validate where
    first _ (Success x)   = Success x
    first f (Failure err) = Failure (f err)
    second = fmap
    bimap f _ (Failure l) = Failure (f l)
    bimap _ g (Success r) = Success (g r)

-- ^ Return success or the accumulation of all failures
instance Monoid a => Applicative (Validate a) where
    pure = Success
    Success f <*> Success x = Success (f x)
    Success _ <*> Failure x = Failure x
    Failure x <*> Failure y = Failure (x <> y)
    Failure x <*> Success _ = Failure x

-- | @[a,b,c]  ==>  a, b, c@
commaSep :: [Doc] -> Doc
commaSep = encloseSep mempty mempty (comma <> space)

-- | Like 'tupled', but comma-space separated.
tupled' :: [Doc] -> Doc
tupled' = encloseSep lparen rparen (comma <> space)

-- | Prefix all contained documents with a bullet symbol.
bulletList :: [Doc] -> Doc
bulletList = align . vsep . map (("  - " <>) . align)

pluralS :: [a] -> Doc
pluralS [_] = ""
pluralS _ = "s"
