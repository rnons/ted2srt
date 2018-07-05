module Static where

import           Crypto.Hash            (Digest, MD5)
import           Crypto.Hash.Conduit    (hashFile)
import qualified Data.ByteArray         as ByteArray
import qualified Data.ByteString        as S
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Char8  as S8
import qualified Data.Map               as M
import           RIO

staticDir :: String
staticDir = "dist"

bundles :: [String]
bundles =
  [ "Home.js"
  ]

type LookupStatic = String -> String

static :: IO LookupStatic
static = do
  cache <- mkHashMap
  pure $ \filename ->
    case (M.lookup filename cache) of
      Nothing   -> filename
      Just hash -> filename <> "?etag=" <> hash

mkHashMap :: IO (M.Map FilePath String)
mkHashMap = do
  mapM hashFile' bundles >>= return . M.fromList
  where
  hashFile' :: String -> IO (FilePath, String)
  hashFile' filename = do
    let filepath = staticDir ++ '/' : filename
    h <- base64md5File filepath
    pure (filename, h)

base64md5File :: FilePath -> IO String
base64md5File = fmap (base64 . encode) . hashFile
    where encode d = ByteArray.convert (d :: Digest MD5)

base64 :: S.ByteString -> String
base64 = map tr
       . take 8
       . S8.unpack
       . Data.ByteString.Base64.encode
  where
    tr '+' = '-'
    tr '/' = '_'
    tr c   = c
