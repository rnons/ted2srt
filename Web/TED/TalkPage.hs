-- | TED talk page module
-- Parse the TED talk page to retrieve information

{-# LANGUAGE OverloadedStrings  #-}
module Web.TED.TalkPage 
  ( getTalkId
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
import           Text.XML.Cursor (Cursor, attribute, attributeIs,
                                  element, fromDocument, ($//), (&|))


-- | Given talk url, return talk id.
getTalkId :: Text -> IO (Maybe Int)
getTalkId uri = E.catch
    (do body <- simpleHttp $ T.unpack uri
        let cursor = fromDocument $ parseLBS body
        return $ Just $ parseId cursor)
    (\e -> do print (e :: E.SomeException)
              return Nothing)

parseId :: Cursor -> Int
parseId cursor = read $ T.unpack $ head $ head $ cursor $// element "div" >=> 
                 attributeIs "id" "share_and_save" &| attribute "data-id"

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
