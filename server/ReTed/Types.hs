{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module ReTed.Types where

import           Data.Aeson
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)
import           Prelude hiding (id)

import qualified Web.TED as TED

mkTalkUrl :: Text -> Text
mkTalkUrl s = "http://www.ted.com/talks/" <> s

marshal :: TED.Talk -> IO RedisTalk
marshal talk = do
    (mediaSlug, mediaPad) <- TED.getSlugAndPad $ mkTalkUrl $ TED.slug talk
    return RedisTalk { id = TED.id talk
                     , name = TED.name talk
                     , description = TED.description talk
                     , slug = TED.slug talk
                     , images = TED.images talk
                     , publishedAt = TED.publishedAt talk
                     , mSlug = mediaSlug
                     , mPad = mediaPad
                     }

data RedisTalk = RedisTalk
    { id            :: Int
    , name          :: Text
    , description   :: Text
    , slug          :: Text
    , images        :: TED.Image
    , publishedAt   :: UTCTime
    , mSlug         :: Text
    , mPad          :: Double
    } deriving (Generic, Show)
instance FromJSON RedisTalk
instance ToJSON RedisTalk

data TalkCache = TalkCache
    { caLanguages   :: [TED.Language]
    , caAudio       :: Bool
    } deriving (Generic, Show)
instance FromJSON TalkCache
instance ToJSON TalkCache

-- data TalkResp = TalkResp RedisTalk [TED.Language]
data TalkResp = TalkResp RedisTalk TalkCache
instance ToJSON TalkResp where
    toJSON (TalkResp talk cache) =
        object [ "id" .= id talk
               , "name" .= name talk
               , "description" .= description talk
               , "slug" .= slug talk
               , "images" .= images talk
               , "publishedAt" .= publishedAt talk
               , "mSlug" .= mSlug talk
               , "languages" .= caLanguages cache
               , "hasAudio" .= caAudio cache
               ]

tedTalkToCache :: TED.Talk -> TalkCache
tedTalkToCache talk =
    TalkCache { caLanguages = TED.languages talk
              , caAudio = TED.talkHasAudio talk
              }
