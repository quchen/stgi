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
