-- | Useful utilities that don't really fit in a specific location.
module Stg.Util (
    show',
    Validate(..),
    takeMatchingLength,
) where



import           Data.Bifunctor
import           Data.Monoid
import           Data.Text      (Text)
import qualified Data.Text      as T



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

-- | Take as many elements from one list as there are in another.
--
-- This is just a lazier version of @\xs ys -> take (length ys) xs@.
takeMatchingLength :: [a] -> [b] -> [a]
takeMatchingLength xs ys = zipWith const xs ys
