{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module ReTed.Types where

import           Data.Aeson
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Prelude hiding (id)

import qualified Web.TED as API

mkTalkUrl :: Text -> Text
mkTalkUrl s = "http://www.ted.com/talks/" <> s

marshal :: API.Talk -> IO RedisTalk
marshal talk = do
    (mediaSlug, mediaPad) <- API.getSlugAndPad $ mkTalkUrl $ API.slug talk
    return RedisTalk { id = API.id talk
                     , name = API.name talk
                     , description = API.description talk
                     , slug = API.slug talk
                     , images = API.images talk
                     , publishedAt = API.publishedAt talk
                     , mSlug = mediaSlug
                     , mPad = mediaPad
                     }

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

data TalkResp = TalkResp RedisTalk [API.Language]
instance ToJSON TalkResp where
    toJSON (TalkResp talk languages) =
        object [ "id" .= id talk
               , "name" .= name talk
               , "description" .= description talk
               , "slug" .= slug talk
               , "images" .= images talk
               , "publishedAt" .= publishedAt talk
               , "mSlug" .= mSlug talk
               , "languages" .= languages
               ]

apiTalkToValue :: API.Talk -> TalkCache
apiTalkToValue talk =
    TalkCache { caLanguages = API.languages talk
              , caAudio = API.talkHasAudio talk
              }
