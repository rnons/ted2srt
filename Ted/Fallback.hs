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

data Talk = Talk 
    { tid               :: Text
    , title             :: Text
    , subLang           :: [(Text, Text)]
    , subName           :: Text
    , subLag            :: Double
    } deriving Show

getTalk :: Text -> IO (Maybe Talk)
getTalk uri = E.catch
    (do body <- simpleHttp $ T.unpack uri
        let cursor = fromDocument $ parseLBS body
        return $ Just Talk { tid = talkIdTitle cursor "data-id"
                           , title = talkIdTitle cursor "data-title"
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

{- List in the form of [("English", "en")] -}
languageCodes :: XC.Cursor -> [(Text, Text)]
languageCodes cursor = zip lang code
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


