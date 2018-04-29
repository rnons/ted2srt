module Models.RedisKeys where

import qualified Data.ByteString.Char8 as C
import           Data.Text             (Text, unpack)
import           RIO
import           Text.Printf.TH        (sb)


cache :: Int -> C.ByteString
cache = [sb|cache:%d|]

slug :: Text -> C.ByteString
slug = [sb|%s|] . unpack
