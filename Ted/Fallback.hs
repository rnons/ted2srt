{-# LANGUAGE OverloadedStrings  #-}
module Ted.Fallback where

import           Control.Exception as E
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text (Text)
import           qualified Data.Text as T
import           Network.HTTP.Conduit
import           Text.HTML.DOM (parseLBS)
import           Text.Regex.Posix ((=~))
import           Text.XML (Name)
import           Text.XML.Cursor (attribute, attributeIs, element, fromDocument,
                                  ($//), (&|), (&//))
import qualified Text.XML.Cursor as XC

import Ted.Types (SubTalk(..))

getTalk :: Text -> IO (Maybe SubTalk)
getTalk uri = E.catch
    (do body <- simpleHttp $ T.unpack uri
        let cursor = fromDocument $ parseLBS body
        return $ Just SubTalk { tid = read $ T.unpack $ talkIdTitle cursor "data-id"
                           , title = talkIdTitle cursor "data-title"
                           , intro = ""
                           , link = uri
                           , subLang = languageCodes cursor
                           , subName = mediaSlug body
                           , subLag = mediaPad body
                           })
    (\e -> do print (e :: E.SomeException)
              return Nothing)

talkIdTitle :: XC.Cursor -> Name -> Text
talkIdTitle cursor name = attr name
  where
    cur = head $ cursor $// element "div" >=> attributeIs "id" "share_and_save"
    attr name = head $ attribute name cur

-- List in the form of [("en", "English")]
languageCodes :: XC.Cursor -> [(Text, Text)]
languageCodes cursor = zip code lang
  where
    lang = cursor $// element "select" 
                  >=> attributeIs "name" "languageCode" 
                  &// element "option" &// XC.content
    code = concat $ cursor $// element "select" 
                           >=> attributeIs "name" "languageCode" 
                           &// element "option"
                           &| attribute "value"

{- File name when saved to local. -}
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
