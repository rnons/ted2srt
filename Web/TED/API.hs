-- | TED API module
-- Documented at <http://developer.ted.com/io-docs>

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.TED.API
  ( Talk (..)
  , SearchTalk (..)
  , Theme (..)
  , TM (..)
  , Sp (..)
  , Speaker (..)
  , Img (..)
  , Tag (..)
  , queryTalk
  , searchTalk
  , talkImg
  , talkLanguages
  ) where

import           Data.Aeson
import qualified Data.ByteString.Char8 as B8
import           Data.Aeson.Types (defaultOptions, Options(..))
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit (simpleHttp)
import           Network.HTTP.Types (urlEncode)

-- | Response of https://api.ted.com/v1/talks/:id.json
data QueryResponse = QueryResponse
    { talk          :: Talk
    } deriving (Generic, Show)
instance FromJSON QueryResponse

-- | Some talks (performance) has no language infomation e.g. 581.
data Talk = Talk
    { _id           :: Int
    , _name         :: Text
    , _description  :: Text
    , _slug         :: Text
    , _recorded_at  :: Text
    , _published_at :: Text
    , _updated_at   :: Text
    , _viewed_count :: Int
    , _images       :: [Image]
    , _languages    :: Maybe Value
    , _tags         :: [Tag]
    , _themes       :: [Theme]
    , _speakers     :: [Speaker]
    } deriving (Generic, Show)
instance FromJSON Talk where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

data Image = Image
    { _image        :: Img
    } deriving (Generic, Show)
instance FromJSON Image where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
data Img = Img
    { size          :: Text
    , url           :: Text
    } deriving (Generic, Show)
instance FromJSON Img

data Tag = Tag
    { tag           :: Text
    } deriving (Generic, Show)
instance FromJSON Tag

data Theme = Theme
    { theme         :: TM
    } deriving (Generic, Show)
instance FromJSON Theme
data TM = TM
    { tm_id         :: Int
    , tm_name       :: Text
    } deriving (Generic, Show)
instance FromJSON TM where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 3 }

data Speaker = Speaker
    { speaker       :: Sp
    } deriving (Generic, Show)
instance FromJSON Speaker
data Sp = Sp
    { sp_id         :: Int
    , sp_name       :: Text
    } deriving (Generic, Show)
instance FromJSON Sp where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 3 }

-- | Response of https://api.ted.com/v1/search.json
data SearchResponse = SearchResponse
    { results       :: [SearchResult]
    } deriving (Generic, Show)
instance FromJSON SearchResponse

data SearchResult = SearchResult
    { _talk       :: SearchTalk
    } deriving (Generic, Show)
instance FromJSON SearchResult where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

data SearchTalk = SearchTalk
    { s_id          :: Int
    , s_name        :: Text
    , s_description :: Text
    , s_slug        :: Text
    , s_recorded_at :: Text
    , s_published_at:: Text
    , s_updated_at  :: Text
    } deriving (Generic, Show)
instance FromJSON SearchTalk where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }

queryTalk :: Int -> IO Talk
queryTalk tid = do
    res <- simpleHttp rurl
    case eitherDecode res of
        Right r -> return $ talk r
        Left er -> error er

  where
    rurl = "https://api.ted.com/v1/talks/" ++ show tid ++
           ".json?api-key=2a9uggd876y5qua7ydghfzrq"

-- | "languages": { "en": { "name": "English", "native": true } }
talkLanguages :: Talk -> [(Text, Text)]
talkLanguages t =
    case _languages t of
        Just (Object langs) ->
            let langCode = HM.keys langs
                langName = map ((\(String str) -> str) . (\(Object hm) -> hm HM.! "name"))
                               (HM.elems langs)
            in  zip langCode langName
        _                   -> []

-- | "images": { ["image": { "size": , "url": }] }
talkImg :: Talk -> Text
talkImg t = url $ _image (_images t !! 1)

searchTalk :: B8.ByteString -> IO [SearchTalk]
searchTalk q = do
    res <- simpleHttp rurl
    case eitherDecode res of
        Right r -> return $ map _talk (results r)
        Left er -> error er
  where
    query = B8.unpack $ urlEncode True q
    rurl = "https://api.ted.com/v1/search.json?q=" ++ query ++
           "&categories=talks&api-key=2a9uggd876y5qua7ydghfzrq"

