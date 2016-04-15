-- | TED talk page module
-- Parse the TED talk page to retrieve information

{-# LANGUAGE OverloadedStrings  #-}
module Web.TED.TalkPage
  ( getTalkId
  , getSlugAndPad
  , parseDescription
  , parseTalkObject
  ) where

import           Control.Exception as E
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text (Text)
import           qualified Data.Text as T
import           Network.HTTP.Conduit
import           Prelude hiding (id)
import           Text.HTML.DOM (parseLBS)
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

parseDescription :: L8.ByteString -> Text
parseDescription body = head $ head $
    cursor $// element "meta" &.// attributeIs "name" "description"
                              &| attribute "content"
  where
    cursor = fromDocument $ parseLBS body

parseTalkObject :: L8.ByteString -> L8.ByteString
parseTalkObject body = L8.pack $ last $ last r
  where
    pat = "talkPage.init\",(.+))</script>" :: String
    r = L8.unpack body =~ pat :: [[String]]

-- | Given talk url, return mediaSlug and mediaPad of talk.
getSlugAndPad :: Text -> IO (Text, Double)
getSlugAndPad rurl = E.catch
    (do body <- simpleHttp $ T.unpack rurl
        return (mediaSlug body, mediaPad body)
    )
    (\e -> error $ show (e :: E.SomeException))

-- File name slug when saved to local.
mediaSlug :: L8.ByteString -> Text
mediaSlug body = T.pack $ last $ last r
  where
    pat = "\"file\":\"http://download.ted.com/talks/(.+)-320k.mp4\\?dnt" :: String
    r = L8.unpack body =~ pat :: [[String]]

-- TED talk videos begin with different versions of TED promos.
-- To keep sync, add time delay (in milliseconds) to subtitles.
mediaPad :: L8.ByteString -> Double
mediaPad body = read t * 1000.0
  where
    pat = "introDuration\":([^,]+)" :: String
    r = L8.unpack body =~ pat :: [[String]]
    t = last $ last r
