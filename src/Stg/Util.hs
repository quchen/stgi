-- | Useful utilities that don't really fit in a specific location.
module Stg.Util (
    show',
    Validate(..),

    -- * Prettyprinter extensions
    commaSep,
) where



import           Data.Bifunctor
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc



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
    Failure x <*> Failure y = Failure (x `mappend` y)
    Failure x <*> Success _ = Failure x

-- | @[a,b,c]  ==>  a, b, c@
commaSep :: [Doc ann] -> Doc ann
commaSep = encloseSep mempty mempty (comma <> space)
