{-# LANGUAGE OverloadedStrings #-}

-- | Useful utilities that don't really fit in a specific location.
module Stg.Util (
    show',
    Validate(..),

    -- * Prettyprinter extensions
    commaSep,
    spaceSep,
    bulletList,
    pluralS,
) where



import           Data.Bifunctor
import           Data.Monoid
import           Data.String
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



-- | 'Either' with an accumulating 'Applicative' instance
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

-- | Return success or the accumulation of all failures
instance Monoid a => Applicative (Validate a) where
    pure = Success
    Success f <*> Success x = Success (f x)
    Success _ <*> Failure x = Failure x
    Failure x <*> Failure y = Failure (x <> y)
    Failure x <*> Success _ = Failure x

-- | @[a,b,c]  ==>  a, b, c@
commaSep :: Pretty a => [a] -> Doc
commaSep = encloseSep mempty mempty (comma <> space) . map pretty

-- | @[a,b,c]  ==>  a b c@
spaceSep :: Pretty a => [a] -> Doc
spaceSep = hsep . map pretty

-- | Prefix all contained documents with a bullet symbol.
bulletList :: Pretty a => [a] -> Doc
bulletList = align . vsep . map (("  - " <>) . align . pretty)

-- | Add an \'s' for non-singleton lists.
pluralS :: IsString string => [a] -> string
pluralS [_] = ""
pluralS _ = "s"
