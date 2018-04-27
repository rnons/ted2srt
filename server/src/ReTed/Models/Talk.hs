{-# LANGUAGE ScopedTypeVariables #-}
module ReTed.Models.Talk where

import           Control.Exception                  (handle)
import           Control.Monad                      (forM, liftM, mzero, void)
import           Data.Aeson
import qualified Data.ByteString.Char8              as C
import           Data.Maybe                         (catMaybes)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T
import           Data.Time                          (UTCTime)
import           Data.Time.Clock.POSIX              (posixSecondsToUTCTime)
import           Database.Beam
import           Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple         as DB
import           Database.PostgreSQL.Simple.SqlQQ   (sql)
import qualified Database.PostgreSQL.Simple.ToField as DB
import qualified Database.Redis                     as KV
import           GHC.Generics                       (Generic)
import           Network.HTTP.Conduit               (HttpException, simpleHttp)
import           Prelude                            hiding (id)
import           Text.HTML.DOM                      (parseLBS)
import           Text.XML.Cursor                    (fromDocument)

import           Model                              (Language (..), Talk,
                                                     TalkT (..), talkDb, _talks)
import           ReTed.Config                       (Config (..))
import qualified ReTed.Models.RedisKeys             as Keys
import           ReTed.Types                        (mkTalkUrl)
import           Web.TED                            (FileType (..),
                                                     Subtitle (..), toSub)
import qualified Web.TED.API                        as API
import           Web.TED.TalkPage                   (parseDescription,
                                                     parseImage, parseMediaPad,
                                                     parseMediaSlug,
                                                     parseTalkObject)
import           Web.TED.Types                      (SearchTalk (s_slug))


data TalkObj = TalkObj
    { oId          :: Int
    , oName        :: Text
    , oSlug        :: Text
    , oFilmedAt    :: UTCTime
    , oPublishedAt :: UTCTime
    , oLanguages   :: [Language]
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


getTalks :: DB.Connection -> Int -> IO [Talk]
getTalks conn limit = do
  runBeamPostgres conn $ do
    runSelectReturningList $ select
      (limit_ (fromIntegral limit)
       $ orderBy_ (\t -> desc_ (_talkId t)) $ all_ (_talks talkDb))

getTalk :: Config -> Int -> Text -> IO (Maybe Talk)
getTalk config tid url = do
    cached <- KV.runRedis kv $
        KV.get $ Keys.cache tid
    case cached of
        Right (Just _) -> getTalkById config tid (Just url)
        Right Nothing  -> saveToDB config url
        Left _         -> saveToDB config url
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
        _      -> maybe (return Nothing) (saveToDB config) mUrl
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
        Just talk@Talk {..} -> do
            void $ KV.runRedis kv $ KV.multiExec $ do
                void $ KV.setex (Keys.cache _talkId) (3600*24) ""
                KV.set (Keys.slug _talkSlug) (C.pack $ show _talkId)
            let jsonLang = DB.toJSONField _talkLanguages
            void $ DB.execute conn [sql|
                INSERT INTO talks (id, name, slug, description, image, filmed,
                                   published, languages, media_slug, media_pad)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT (id) DO UPDATE
                SET languages = EXCLUDED.languages,
                    media_slug = EXCLUDED.media_slug,
                    media_pad = EXCLUDED.media_pad
                |] (_talkId, _talkName, _talkSlug, _talkDescription, _talkImage, _talkFilmed,
                    _talkPublished, jsonLang, _talkMediaSlug, _talkMediaPad)
            saveTranscriptIfNotAlready config talk
            return $ Just talk
        Nothing -> return Nothing
  where
    conn = dbConn config
    kv = kvConn config

saveTranscriptIfNotAlready :: Config -> Talk -> IO ()
saveTranscriptIfNotAlready config Talk {..} = do
    xs <- DB.query conn [sql|
        SELECT id FROM transcripts
        WHERE id = ?
        |] [_talkId]
    case xs of
        [DB.Only (_::Int)] -> return ()
        _   -> do
            path <- toSub $
                Subtitle 0 _talkSlug ["en"] _talkMediaSlug _talkMediaPad TXT
            case path of
                Just path' -> do
                    transcript <- T.drop 2 <$> T.readFile path'
                    void $ DB.execute conn [sql|
                        INSERT INTO transcripts (id, name, en, en_tsvector)
                        VALUES (?, ?, ?, to_tsvector('english', ? || ?))
                    |] (_talkId, _talkName, transcript, _talkName, transcript)
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
            Just talk -> do
                return $ Just Talk { _talkId = oId talk
                                   , _talkName = oName talk
                                   , _talkSlug = oSlug talk
                                   , _talkFilmed = oFilmedAt talk
                                   , _talkPublished = oPublishedAt talk
                                   , _talkDescription = desc
                                   , _talkImage = img
                                   , _talkLanguages = oLanguages talk
                                   , _talkMediaSlug = mdSlug
                                   , _talkMediaPad = mdPad
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
        _      -> return Nothing
  where
    conn = dbConn config

searchTalkFromDb :: Config -> Text -> IO [Talk]
searchTalkFromDb config q = do
    DB.query conn [sql|
        SELECT talks.* FROM talks JOIN transcripts
        ON talks.id = transcripts.id
        WHERE en_tsvector @@
              to_tsquery('english', ?)
        |] [query]
  where
    conn = dbConn config
    query = T.intercalate "&" $ T.words q

searchTalk :: Config -> Text -> IO [Talk]
searchTalk config q =
    handle (\(_::HttpException) -> searchTalkFromDb config q) $ do
        searchResults <- API.searchTalk q
        liftM catMaybes $ forM searchResults $ \t -> do
            getTalkBySlug config (s_slug t)
