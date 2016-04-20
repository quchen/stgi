-- | Useful utilities that don't really fit in a specific location.
module Stg.Util (
    show'
) where



import           Data.Text
import qualified Data.Text as T



-- | 'show' with 'Text' as codomain.
--
-- @
-- show' = 'T.pack' . 'show'
-- @
show' :: Show a => a -> Text
show' = T.pack . show
