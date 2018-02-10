module ReTed.Models.RedisKeys where

import qualified Data.ByteString.Char8 as C
import           Data.Text (Text, unpack)
import           Text.Printf.TH (sb)


cache :: Int -> C.ByteString
cache = [sb|cache:%d|]

slug :: Text -> C.ByteString
slug = [sb|%s|] . unpack
