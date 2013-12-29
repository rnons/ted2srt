-- | TED talk page module
-- Parse the TED talk page to retrieve information

{-# LANGUAGE OverloadedStrings  #-}
module Web.TED.TalkPage 
  ( SubTalk (..)
  , getTalk
  , getSlugAndPad
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
import           Text.XML.Cursor (Cursor, attribute, attributeIs, content,
                                  element, fromDocument, ($//), (&|), (&//))


-- | Data used on @{TalksR}
data SubTalk = SubTalk 
    { id            :: Int
    , name          :: Text
    , image         :: Text
    , description   :: Text
    , link          :: Text
    , languages     :: [(Text, Text)]
    , subSlug       :: Text
    , subLag        :: Double
    } deriving Show

-- Given talk url, get SubTalk.
getTalk :: Text -> IO (Maybe SubTalk)
getTalk uri = E.catch
    (do body <- simpleHttp $ T.unpack uri
        let cursor = fromDocument $ parseLBS body
        return $ Just SubTalk { id = parseId cursor
                              , name = strip $ parseMeta cursor "og:title"
                              , image = small $ parseMeta cursor "og:image"
                              , description = parseMeta cursor "og:description"
                              , link = uri
                              , languages = languageCodes cursor
                              , subSlug = mediaSlug body
                              , subLag = mediaPad body
                              })
    (\e -> do print (e :: E.SomeException)
              return Nothing)
  where
    -- strip the " | Video on TED.com" part
    strip = T.reverse . T.drop 19 . T.reverse
    small url = (T.reverse. T.drop 11 . T.reverse) url `T.append` "240x180.jpg"

parseId :: Cursor -> Int
parseId cursor = read $ T.unpack $ head $ head $ cursor $// element "div" >=> 
                 attributeIs "id" "share_and_save" &| attribute "data-id"

-- <meta property="og:image" 
--   content="http://images.ted.com/images/ted/*_389x292.jpg">
parseMeta :: Cursor -> Text -> Text
parseMeta cursor value = head $ head $ cursor $// element "meta" >=> 
                         attributeIs "property" value &| attribute "content"

-- List in the form of [("en", "English")]
languageCodes :: Cursor -> [(Text, Text)]
languageCodes cursor = zip code lang
  where
    lang = cursor $// element "select" 
                  >=> attributeIs "name" "languageCode" 
                  &// element "option" &// content
    code = concat $ cursor $// element "select" 
                           >=> attributeIs "name" "languageCode" 
                           &// element "option"
                           &| attribute "value"

-- File name slug when saved to local.
mediaSlug :: L8.ByteString -> Text
mediaSlug body = T.pack $ last $ last r
  where
    pat = "mediaSlug\":\"([^\"]+)\"" :: String
    r = L8.unpack body =~ pat :: [[String]]

-- TED talk videos begin with different versions of TED promos. 
-- To keep sync, add time delay (in milliseconds) to subtitles.
mediaPad :: L8.ByteString -> Double
mediaPad body = read t * 1000.0
  where
    pat = "mediaPad\":(.+)}" :: String
    r = L8.unpack body =~ pat :: [[String]]
    t = last $ last r

getSlugAndPad :: Text -> IO (Text, Double)
getSlugAndPad rurl = E.catch
    (do body <- simpleHttp $ T.unpack rurl
        return (mediaSlug body, mediaPad body)
    )
    (\e -> error $ show (e :: E.SomeException))
