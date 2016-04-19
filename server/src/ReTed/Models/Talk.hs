{-# LANGUAGE DeriveGeneric #-}
module ReTed.Models.Talk where

import           Control.Monad (mzero, liftM, void)
import           Data.Aeson
import           Data.Text (Text)
import           qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit (simpleHttp)
import           Prelude hiding (id)
import           Text.HTML.DOM (parseLBS)
import           Text.XML.Cursor (fromDocument)

import Web.TED.TalkPage (parseDescription, parseImage, parseTalkObject)


data Language = Language
    { languageName :: Text
    , languageCode :: Text
    } deriving (Generic, Show)
instance FromJSON Language
instance ToJSON Language


data Talk = Talk
    { id           :: Int
    , name         :: Text
    , slug         :: Text
    , description  :: Text
    , image        :: Text
    , filmedAt     :: UTCTime
    , publishedAt  :: UTCTime
    , languages    :: [Language]
    } deriving (Generic, Show)

data TalkObj = TalkObj
    { oId           :: Int
    , oName         :: Text
    , oSlug         :: Text
    , oFilmedAt     :: UTCTime
    , oPublishedAt  :: UTCTime
    , oLanguages    :: [Language]
    } deriving (Generic, Show)

instance FromJSON TalkObj where
    parseJSON (Object v) =
        TalkObj <$> v .: "id"
             <*> v .: "name"
             <*> v .: "slug"
             <*> liftM posixSecondsToUTCTime (v .: "filmed")
             <*> liftM posixSecondsToUTCTime (v .: "published")
             <*> v .: "languages"
    parseJSON _          = mzero


data TalkObjs = TalkObjs
    { talks :: [TalkObj]
    } deriving (Generic, Show)

instance FromJSON TalkObjs


saveToDB :: DB.Connection -> Int -> Text -> IO ()
saveToDB conn tid url = do
    Talk {id, name, slug, description, image, filmedAt, publishedAt, languages}  <-
        fetchTalk tid url
    void $ DB.execute conn [sql|
        INSERT INTO talks (id, name, slug, description, image, filmed, published, languages)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?)
        |] (id, name, slug, description, image, filmedAt, publishedAt, DB.toJSONField languages)

fetchTalk :: Int -> Text -> IO Talk
fetchTalk tid url = do
    body <- simpleHttp $ T.unpack url
    let cursor = fromDocument $ parseLBS body
        desc = parseDescription cursor
        img = parseImage cursor
    let core = parseTalkObject body
    case decode core of
        Just tks -> do
            let talk = head $ talks tks
            return Talk { id = oId talk
                        , name = oName talk
                        , slug = oSlug talk
                        , filmedAt = oFilmedAt talk
                        , publishedAt = oPublishedAt talk
                        , description = desc
                        , image = img
                        , languages = oLanguages talk
                        }
        _      -> error "parse error"
