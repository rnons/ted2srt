-- | TED talk page module
-- Parse the TED talk page to retrieve information

{-# LANGUAGE OverloadedStrings #-}
module Web.TED.TalkPage
  ( getTalkId
  , getSlugAndPad
  , parseDescription
  , parseImage
  , parseMediaPad
  , parseMediaSlug
  , parseTalkObject
  ) where

import           Control.Exception          as E
import           Control.Monad
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Network.HTTP.Conduit
import           Prelude                    hiding (id)
import           Text.Regex.Posix           ((=~))
import           Text.XML.Cursor


-- | Given talk url, return talk id.
getTalkId :: Text -> IO (Maybe Int)
getTalkId uri = E.catch
    (do body <- simpleHttp $ T.unpack uri
        return $ Just $ parseId body)
    (\e -> do print (e :: E.SomeException)
              return Nothing)

parseId :: ByteString -> Int
parseId body = read $ L8.unpack $ last $ last r
  where
    pat = "id\":([^,]+),\"duration" :: ByteString
    r = body =~ pat :: [[ByteString]]

parseDescription :: Cursor -> Text
parseDescription cursor = head $ head $
    cursor $// element "meta" &.// attributeIs "name" "description"
                              &| attribute "content"

parseImage :: Cursor -> Text
parseImage cursor = head $ head $
    cursor $// element "meta" &.// attributeIs "property" "og:image:secure_url"
                              &| attribute "content"

parseTalkObject :: ByteString -> ByteString
parseTalkObject body = last $ last r
  where
    pat = "player_talks\":\\[(.+)\\],\"recorded_at" :: ByteString
    r = body =~ pat :: [[ByteString]]

-- | Given talk url, return mediaSlug and mediaPad of talk.
getSlugAndPad :: Text -> IO (Text, Double)
getSlugAndPad rurl = E.catch
    (do body <- simpleHttp $ T.unpack rurl
        return (parseMediaSlug body, parseMediaPad body)
    )
    (\e -> error $ show (e :: E.SomeException))

-- File name slug when saved to local.
parseMediaSlug :: ByteString -> Text
parseMediaSlug body = T.pack $ L8.unpack $ last $ last r
  where
    pat = "\"low\":\"https://download.ted.com/talks/(.+)-light.mp4\\?apikey=" :: ByteString
    r = body =~ pat :: [[ByteString]]

-- TED talk videos begin with different versions of TED promos.
-- To keep sync, add time delay (in milliseconds) to subtitles.
parseMediaPad :: ByteString -> Double
parseMediaPad body = read t * 1000.0
  where
    pat = "introDuration\":([^,]+)" :: ByteString
    r = body =~ pat :: [[ByteString]]
    t = L8.unpack $ last $ last r
