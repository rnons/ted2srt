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
  , talkHasAudio
  ) where

import           Control.Applicative ((<$>), (<*>))
import qualified Control.Exception as E
import           Control.Monad (liftM, mzero)
import           Data.Aeson
import qualified Data.ByteString.Char8 as B8
import           Data.Aeson.Types (defaultOptions, Options(..))
import qualified Data.HashMap.Strict as HM
import           Data.List (sort)
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.Time.Format (parseTimeM, defaultTimeLocale)
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
    { id           :: Int
    , name         :: Text
    , description  :: Text
    , slug         :: Text
    , recorded_at  :: Text
    , published_at :: Maybe UTCTime
    , updated_at   :: Text
    , viewed_count :: Int
    , images       :: [Image]
    , media        :: Value
    , languages    :: Maybe Value
    , tags         :: [Tag]
    , themes       :: [Theme]
    , speakers     :: [Speaker]
    } deriving (Generic, Show)
instance FromJSON Talk where
    parseJSON (Object v) = Talk <$>
                           v .: "id" <*>
                           v .: "name" <*>
                           v .: "description" <*>
                           v .: "slug" <*>
                           v .: "recorded_at" <*>
                           liftM parseUTime (v .: "published_at") <*>
                           v .: "updated_at" <*>
                           v .: "viewed_count" <*>
                           v .: "images" <*>
                           v .: "media" <*>
                           v .:? "languages" <*>
                           v .: "tags" <*>
                           v .: "themes" <*>
                           v .: "speakers"
      where
        parseUTime :: String -> Maybe UTCTime
        parseUTime = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
    parseJSON _          = mzero

data Image = Image
    { image        :: Img
    } deriving (Generic, Show)
instance FromJSON Image

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

-- | Return Nothing for talks hosted externally (youtube, vimeo), e.g. 720.
-- They have no downloadable subtitles and will fail getSlugAndPad.
queryTalk :: Int -> IO (Maybe Talk)
queryTalk tid = E.catch
    (do
        res <- simpleHttp rurl
        case eitherDecode res of
            Right r -> return $ Just $ talk r
            Left er -> error er)
    (\e -> print (e :: E.SomeException) >> return Nothing)
  where
    rurl = "http://api.ted.com/v1/talks/" ++ show tid ++
           ".json?external=false&podcasts=true&api-key=2a9uggd876y5qua7ydghfzrq"

-- | "languages": { "en": { "name": "English", "native": true } }
talkLanguages :: Talk -> [(Text, Text)]
talkLanguages t =
    case languages t of
        Just (Object langs) ->
            let langCode = HM.keys langs
                langName = map ((\(String str) -> str) . (\(Object hm) -> hm HM.! "name"))
                               (HM.elems langs)
            in  sort $ zip langName langCode
        _                   -> []

-- | Whether "audio-podcast" field is present
talkHasAudio :: Talk -> Bool
talkHasAudio t = 
    case media t of
        Object ms -> isJust $ HM.lookup "internal" ms >>=
                            \(Object im) -> HM.lookup "audio-podcast" im 
        _         -> False

-- | "images": { ["image": { "size": , "url": }] }
talkImg :: Talk -> Text
talkImg t = url $ image (images t !! 1)

searchTalk :: Text -> IO [SearchTalk]
searchTalk q = do
    res <- simpleHttp rurl
    case eitherDecode res of
        Right r -> return $ map _talk (results r)
        Left er -> error er
  where
    query = B8.unpack $ urlEncode True $ B8.pack $ T.unpack q
    rurl = "http://api.ted.com/v1/search.json?q=" <> query <>
           "&categories=talks&api-key=2a9uggd876y5qua7ydghfzrq"

