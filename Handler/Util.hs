{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Handler.Util where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Time (UTCTime)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Prelude hiding (id)
import           System.Directory

import qualified Web.TED as API

tedTalkUrl :: Text -> Text
tedTalkUrl s = "http://www.ted.com/talks/" <> s

marshal :: API.Talk -> IO RedisTalk
marshal talk = do
    (mediaSlug, mediaPad) <- API.getSlugAndPad $ tedTalkUrl $ API.slug talk
    return RedisTalk { id = API.id talk
                     , name = API.name talk
                     , description = API.description talk
                     , slug = API.slug talk
                     , image = API.talkImg talk
                     , publishedAt = API.published_at talk
                     , mSlug = mediaSlug
                     , mPad = mediaPad
                     }

talkUrl, newTalkUrl :: Text
talkUrl = "http://www.ted.com/talks/"
newTalkUrl = "http://new.ted.com/talks/"

downloadUrl :: Text
downloadUrl = "http://download.ted.com/talks/"

-- Available quality: 1500k, 950k, 600k, 450k, 320k, 180k, 64k
mediaUrl :: Text -> Text -> Text
mediaUrl part quality =
  downloadUrl <> part <> "-" <> quality <> ".mp4"

img113x85 :: Text -> Text
img113x85 rurl = (T.reverse. T.drop 11 . T.reverse) rurl <> "113x85.jpg"

-- Drop the talkUrl part.
-- e.g. marla_spivak_why_bees_are_disappearing.html
rewriteUrl ::Text -> Text
rewriteUrl = T.drop $ T.length talkUrl

jsonPath :: Int -> IO (FilePath, Bool)
jsonPath tid = do
    pwd <- getCurrentDirectory
    let path = pwd ++ "/static/json/" ++ show tid ++ ".json"
    cached <- doesFileExist path
    return (path, cached)

data RedisTalk = RedisTalk
    { id            :: Int
    , name          :: Text
    , description   :: Text
    , slug          :: Text
    , image         :: Text
    , publishedAt   :: Maybe UTCTime
    , mSlug         :: Text
    , mPad          :: Double
    } deriving (Generic, Show)
instance FromJSON RedisTalk
instance ToJSON RedisTalk

data TalkCache = TalkCache
    { caLanguages   :: [(Text, Text)]
    , caAudio       :: Bool
    } deriving (Generic, Show)
instance FromJSON TalkCache
instance ToJSON TalkCache

apiTalkToValue :: API.Talk -> TalkCache
apiTalkToValue talk =
    TalkCache { caLanguages = API.talkLanguages talk
              , caAudio = API.talkHasAudio talk
              }
