{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ReTed.Models.Talk where

import           Control.Exception (handle)
import           Control.Monad (mzero, liftM, void)
import           Data.Aeson
import qualified Data.ByteString.Char8 as C
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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

import Web.TED.TalkPage (parseDescription, parseImage, parseTalkObject,
                         parseMediaPad, parseMediaSlug)
import ReTed.Config (Config(..))
import ReTed.Types (mkTalkUrl)
import qualified ReTed.Models.RedisKeys as Keys
import Web.TED (FileType(..), Subtitle(..), toSub)


data Language = Language
    { languageName  :: Text
    , languageCode  :: Text
    , endonym       :: Text
    } deriving (Generic, Show)
instance FromJSON Language
instance ToJSON Language

instance DB.FromField [Language] where
    fromField = DB.fromJSONField


data Talk = Talk
    { id            :: Int
    , name          :: Text
    , slug          :: Text
    , filmedAt      :: UTCTime
    , publishedAt   :: UTCTime
    , description   :: Text
    , image         :: Text
    , languages     :: [Language]
    , mediaSlug     :: Text
    , mediaPad      :: Double
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
        ORDER BY id DESC
        LIMIT ?
        |] [limit]

getTalk :: Config -> Int -> Text -> IO (Maybe Talk)
getTalk config tid url = do
    cached <- KV.runRedis kv $
        KV.get $ Keys.cache tid
    case cached of
        Right _ -> getTalkById config tid (Just url)
        Left _ -> saveToDB config url
  where
    kv = kvConn config

getTalkById :: Config -> Int -> Maybe Text -> IO (Maybe Talk)
getTalkById config tid mUrl = do
    xs <- DB.query conn [sql|
        SELECT * FROM talks
        WHERE id = ?
        |] [tid]
    case xs of
        [talk] -> return $ Just talk
        _ -> maybe (return Nothing) (saveToDB config) mUrl
  where
    conn = dbConn config

getTalkBySlug :: Config -> Text -> IO (Maybe Talk)
getTalkBySlug config slug = do
    emtid <- KV.runRedis kv $ KV.get $ Keys.slug slug
    case emtid of
        Right (Just tid) -> do
            getTalk config (read $ C.unpack tid) url
        Right Nothing -> do
            saveToDB config url
        Left _ -> return Nothing
  where
    url = mkTalkUrl slug
    kv = kvConn config

saveToDB :: Config -> Text -> IO (Maybe Talk)
saveToDB config url = do
    mTalk <- fetchTalk url
    case mTalk of
        Just talk@Talk {id, name, slug, description, image, filmedAt,
                        publishedAt, languages, mediaSlug, mediaPad} -> do
            KV.runRedis kv $ KV.multiExec $ do
                KV.setex (Keys.cache id) (3600*24) ""
                KV.set (Keys.slug slug) (C.pack $ show id)
            let jsonLang = DB.toJSONField languages
            DB.execute conn [sql|
                INSERT INTO talks (id, name, slug, description, image, filmed,
                                   published, languages, media_slug, media_pad)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT (id) DO UPDATE
                SET languages = EXCLUDED.languages,
                    media_slug = EXCLUDED.media_slug,
                    media_pad = EXCLUDED.media_pad
                |] (id, name, slug, description, image, filmedAt,
                    publishedAt, jsonLang, mediaSlug, mediaPad)
            saveTranscriptIfNotAlready config talk
            return $ Just talk
        Nothing -> return Nothing
  where
    conn = dbConn config
    kv = kvConn config

saveTranscriptIfNotAlready :: Config -> Talk -> IO ()
saveTranscriptIfNotAlready config Talk {id, name, slug, mediaSlug, mediaPad} = do
    xs <- DB.query conn [sql|
        SELECT id FROM transcripts
        WHERE id = ?
        |] [id]
    case xs of
        [DB.Only (_::Int)] -> return ()
        _   -> do
            path <- toSub $
                Subtitle 0 slug ["en"] mediaSlug mediaPad TXT
            case path of
                Just path' -> do
                    transcript <- T.drop 2 <$> T.readFile path'
                    void $ DB.execute conn [sql|
                        INSERT INTO transcripts (id, name, en, en_tsvector)
                        VALUES (?, ?, ?, to_tsvector('english', ? || ?))
                    |] (id, name, transcript, name, transcript)
                Nothing -> return ()
  where
    conn = dbConn config

fetchTalk :: Text -> IO (Maybe Talk)
fetchTalk url = do
    handle (\(_::HttpException) -> return Nothing) $ do
        body <- simpleHttp $ T.unpack url
        let cursor = fromDocument $ parseLBS body
            desc = parseDescription cursor
            img = parseImage cursor
            mdSlug = parseMediaSlug body
            mdPad = parseMediaPad body
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
                                   , mediaSlug = mdSlug
                                   , mediaPad = mdPad
                                   }
            _      -> error "parse error"

getRandomTalk :: Config -> IO (Maybe Talk)
getRandomTalk config = do
    xs <- DB.query_ conn [sql|
        SELECT * FROM talks
        TABLESAMPLE SYSTEM (1)
        LIMIT 1
        |]
    case xs of
        [talk] -> return $ Just talk
        _ -> return Nothing
  where
    conn = dbConn config

searchTalk :: Config -> Text -> IO [Talk]
searchTalk config q = do
    DB.query conn [sql|
        SELECT talks.* FROM talks JOIN transcripts
        ON talks.id = transcripts.id
        WHERE en_tsvector @@
              to_tsquery('english', ?)
        |] [query]
  where
    conn = dbConn config
    query = T.intercalate "&" $ T.words q
