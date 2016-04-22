module ReTed.Models.RedisKeys where

import Text.Printf.TH (sb)
import qualified Data.ByteString.Char8 as C


cache :: Int -> C.ByteString
cache = [sb|cache:%d|]
