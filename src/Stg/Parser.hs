module Stg.Parser (parse, stg) where



import           Data.Text              (Text)
import qualified Stg.Parser.Parser      as P
import           Stg.Parser.QuasiQuoter (stg)

import           Stg.Language


-- | Parse an STG program.
parse :: Text -> Either Text Program
parse = P.parse P.program
