{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ReTed.Models.Talk where

import           Control.Exception (handle)
import           Control.Monad (mzero, liftM, void)
import           Data.Aeson
import qualified Data.ByteString.Char8 as C
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Database.PostgreSQL.Simple as DB
import qualified Database.PostgreSQL.Simple.FromField as DB
import qualified Database.PostgreSQL.Simple.ToField as DB
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.Redis as KV
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit (HttpException, simpleHttp)
import           Prelude hiding (id)
import           Text.HTML.DOM (parseLBS)
import           Text.XML.Cursor (fromDocument)

import Web.TED.TalkPage (parseDescription, parseImage, parseTalkObject)
import ReTed.Config (Config(..))


data Language = Language
    { languageName :: Text
    , languageCode :: Text
    } deriving (Generic, Show)
instance FromJSON Language
instance ToJSON Language

instance DB.FromField [Language] where
    fromField = DB.fromJSONField


data Talk = Talk
    { id           :: Int
    , name         :: Text
    , slug         :: Text
    , filmedAt     :: UTCTime
    , publishedAt  :: UTCTime
    , description  :: Text
    , image        :: Text
    , languages    :: [Language]
    } deriving (Generic, Show)

instance DB.FromRow Talk
instance ToJSON Talk


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


getTalks :: DB.Connection -> Int -> IO [Talk]
getTalks conn limit = do
    DB.query conn [sql|
        SELECT * FROM talks
        LIMIT ?
        |] [limit]

getTalk :: Config -> Int -> Text -> IO (Maybe Talk)
getTalk config tid url = do
    cached <- KV.runRedis kv $ do
        KV.get ("cache:" <> C.pack (show tid))
    case cached of
        Right _ -> do
            xs <- DB.query conn [sql|
                SELECT * FROM talks
                WHERE id = ?
                |] [tid]
            case xs of
                [talk] -> return $ Just talk
                _ -> do
                    mTalk <- saveToDB config tid url
                    return mTalk
        Left _ -> do
            mTalk <- saveToDB config tid url
            return mTalk
  where
    conn = dbConn config
    kv = kvConn config

saveToDB :: Config -> Int -> Text -> IO (Maybe Talk)
saveToDB config tid url = do
    mTalk <- fetchTalk tid url
    case mTalk of
        Just talk@Talk {id, name, slug, description, image, filmedAt, publishedAt, languages} -> do
            KV.runRedis kv $ do
                KV.setex ("cache:" <> C.pack (show tid)) (3600*24) ""
            void $ DB.execute conn [sql|
                INSERT INTO talks (id, name, slug, description, image, filmed, published, languages)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT (id) DO UPDATE SET languages = ?
                |] (id, name, slug, description, image, filmedAt, publishedAt, DB.toJSONField languages, DB.toJSONField languages)
            return $ Just talk
        Nothing -> return Nothing
  where
    conn = dbConn config
    kv = kvConn config

fetchTalk :: Int -> Text -> IO (Maybe Talk)
fetchTalk tid url = do
    handle (\(_::HttpException) -> return Nothing) $ do
        body <- simpleHttp $ T.unpack url
        let cursor = fromDocument $ parseLBS body
            desc = parseDescription cursor
            img = parseImage cursor
        let core = parseTalkObject body
        case decode core of
            Just tks -> do
                let talk = head $ talks tks
                return $ Just Talk { id = oId talk
                                   , name = oName talk
                                   , slug = oSlug talk
                                   , filmedAt = oFilmedAt talk
                                   , publishedAt = oPublishedAt talk
                                   , description = desc
                                   , image = img
                                   , languages = oLanguages talk
                                   }
            _      -> error "parse error"
