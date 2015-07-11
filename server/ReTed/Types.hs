{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module ReTed.Types where

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
                     , images = API.images talk
                     , publishedAt = API.publishedAt talk
                     , mSlug = mediaSlug
                     , mPad = mediaPad
                     }

talkUrl :: Text
talkUrl = "http://www.ted.com/talks/"

data RedisTalk = RedisTalk
    { id            :: Int
    , name          :: Text
    , description   :: Text
    , slug          :: Text
    , images        :: API.Image
    , publishedAt   :: UTCTime
    , mSlug         :: Text
    , mPad          :: Double
    } deriving (Generic, Show)
instance FromJSON RedisTalk
instance ToJSON RedisTalk

data TalkCache = TalkCache
    { caLanguages   :: [API.Language]
    , caAudio       :: Bool
    } deriving (Generic, Show)
instance FromJSON TalkCache
instance ToJSON TalkCache

data TalkResp = TalkResp
    { talk :: RedisTalk
    , languages :: Maybe [API.Language]
    } deriving (Generic, Show)
instance FromJSON TalkResp
instance ToJSON TalkResp

apiTalkToValue :: API.Talk -> TalkCache
apiTalkToValue talk =
    TalkCache { caLanguages = API.languages talk
              , caAudio = API.talkHasAudio talk
              }
