-- | Useful utilities that don't really fit in a specific location.
module Stg.Util (
    show',
    Validate(..)
) where



import           Data.Monoid
import           Data.Text
import qualified Data.Text   as T



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

-- ^ Return success or the accumulation of all failures
instance Monoid a => Applicative (Validate a) where
    pure = Success
    Success f <*> Success x = Success (f x)
    Success _ <*> Failure x = Failure x
    Failure x <*> Failure y = Failure (x <> y)
    Failure x <*> Success _ = Failure x
