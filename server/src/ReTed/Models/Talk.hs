{-# LANGUAGE DeriveGeneric #-}
module ReTed.Models.Talk where

import           Control.Monad (mzero, liftM)
import           Data.Aeson
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           GHC.Generics (Generic)

import Web.TED.TalkPage (parseTalkObject)


data Language = Language
    { languageName :: Text
    , languageCode :: Text
    } deriving (Generic, Show)
instance FromJSON Language


data Talk = Talk
    { id           :: Int
    , name         :: Text
    , slug         :: Text
    , filmedAt     :: UTCTime
    , publishedAt  :: UTCTime
    , languages    :: [Language]
    } deriving (Generic, Show)

instance FromJSON Talk where
    parseJSON (Object v) =
        Talk <$> v .: "id"
             <*> v .: "name"
             <*> v .: "slug"
             <*> liftM posixSecondsToUTCTime (v .: "filmed")
             <*> liftM posixSecondsToUTCTime (v .: "published")
             <*> v .: "languages"
    parseJSON _          = mzero


data Talks = Talks
    { talks :: [Talk]
    } deriving (Generic, Show)

instance FromJSON Talks
