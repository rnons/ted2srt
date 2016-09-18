-- | TED talk page module
-- Parse the TED talk page to retrieve information

{-# LANGUAGE OverloadedStrings  #-}
module Web.TED.TalkPage
  ( getTalkId
  , getSlugAndPad
  , parseDescription
  , parseImage
  , parseMediaPad
  , parseMediaSlug
  , parseTalkObject
  ) where

import           Control.Exception as E
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text (Text)
import           qualified Data.Text as T
import           Network.HTTP.Conduit
import           Prelude hiding (id)
import           Text.Regex.Posix ((=~))
import           Text.XML.Cursor


-- | Given talk url, return talk id.
getTalkId :: Text -> IO (Maybe Int)
getTalkId uri = E.catch
    (do body <- simpleHttp $ T.unpack uri
        return $ Just $ parseId body)
    (\e -> do print (e :: E.SomeException)
              return Nothing)

parseId :: L8.ByteString -> Int
parseId body = read $ last $ last r
  where
    pat = "id\":([^,]+),\"duration" :: String
    r = L8.unpack body =~ pat :: [[String]]

parseDescription :: Cursor -> Text
parseDescription cursor = head $ head $
    cursor $// element "meta" &.// attributeIs "name" "description"
                              &| attribute "content"

parseImage :: Cursor -> Text
parseImage cursor = head $ head $
    cursor $// element "meta" &.// attributeIs "property" "og:image:secure_url"
                              &| attribute "content"

parseTalkObject :: L8.ByteString -> L8.ByteString
parseTalkObject body = L8.pack $ last $ last r
  where
    pat = "talkPage.init\",(.+))</script>" :: String
    r = L8.unpack body =~ pat :: [[String]]

-- | Given talk url, return mediaSlug and mediaPad of talk.
getSlugAndPad :: Text -> IO (Text, Double)
getSlugAndPad rurl = E.catch
    (do body <- simpleHttp $ T.unpack rurl
        return (parseMediaSlug body, parseMediaPad body)
    )
    (\e -> error $ show (e :: E.SomeException))

-- File name slug when saved to local.
parseMediaSlug :: L8.ByteString -> Text
parseMediaSlug body = T.pack $ last $ last r
  where
    pat = "\"file\":\"https://download.ted.com/talks/(.+)-320k.mp4\\?dnt" :: String
    r = L8.unpack body =~ pat :: [[String]]

-- TED talk videos begin with different versions of TED promos.
-- To keep sync, add time delay (in milliseconds) to subtitles.
parseMediaPad :: L8.ByteString -> Double
parseMediaPad body = read t * 1000.0
  where
    pat = "introDuration\":([^,]+)" :: String
    r = L8.unpack body =~ pat :: [[String]]
    t = last $ last r
