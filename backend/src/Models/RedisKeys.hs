module Models.RedisKeys where

import qualified Data.ByteString.Char8 as C
import qualified Data.Text             as T
import           RIO


cache :: Int -> C.ByteString
cache  = C.pack . show

slug :: Text -> C.ByteString
slug  = C.pack . T.unpack
