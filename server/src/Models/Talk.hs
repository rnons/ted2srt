module Models.Talk where

import           Control.Monad                    (liftM, mzero, void)
import           Data.Aeson
import qualified Data.ByteString.Char8            as C
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import           Data.Time                        (UTCTime)
import           Data.Time.Clock.POSIX            (posixSecondsToUTCTime)
import           Database.Beam
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres.Full      as Pg
import qualified Database.PostgreSQL.Simple       as DB
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.Redis                   as KV
import           GHC.Generics                     (Generic)
import           Network.HTTP.Conduit             (HttpException, Manager,
                                                   httpLbs, parseUrlThrow,
                                                   responseBody)
import           RIO                              hiding (id)
import           Text.HTML.DOM                    (parseLBS)
import           Text.XML.Cursor                  (fromDocument)

import           Config                           (Config (..))
import           Model
import qualified Models.RedisKeys                 as Keys
import           Models.Types                     (mkTalkUrl)
import           System.IO                        (print)
import           Web.TED                          (FileType (..), Subtitle (..),
                                                   toSub)
import           Web.TED.TalkPage                 (parseDescription, parseImage,
                                                   parseMediaPad,
                                                   parseMediaSlug,
                                                   parseTalkObject)


data TalkObj = TalkObj
  { id          :: Int
  , name        :: Text
  , slug        :: Text
  , filmedAt    :: UTCTime
  , publishedAt :: UTCTime
  , languages   :: [Language]
  } deriving (Generic, Show)

instance FromJSON TalkObj where
  parseJSON (Object v) = TalkObj
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "slug"
    <*> liftM posixSecondsToUTCTime (v .: "filmed")
    <*> liftM posixSecondsToUTCTime (v .: "published")
    <*> v .: "languages"
  parseJSON _          = mzero


getTalks :: DB.Connection -> Int -> IO [Talk]
getTalks conn limit = runBeamPostgres conn $ do
  runSelectReturningList $ select
    ( limit_ (fromIntegral limit)
    $ orderBy_ (\t -> desc_ (_talkId t))
    $ all_ (_talks talkDb)
    )

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
  xs <- runBeamPostgres (dbConn config) $ runSelectReturningOne $ select
    ( filter_ (\talk -> (_talkId talk ==. val_ tid))
    $ all_ (_talks talkDb)
    )
  case xs of
    Just talk -> return $ Just talk
    _         -> maybe (return Nothing) (saveToDB config) mUrl

hush :: Either a b -> Maybe b
hush (Left _)  = Nothing
hush (Right v) = Just v

getTalkBySlug :: Config -> Text -> IO (Maybe Talk)
getTalkBySlug config slug = do
  mtid <- fmap (join . hush) <$> KV.runRedis kv $ KV.get $ Keys.slug slug
  case mtid of
    Just tid ->
      case readMaybe $ C.unpack tid of
        Just tid' -> getTalk config tid' url
        Nothing   -> pure Nothing
    Nothing  ->
      saveToDB config url
  where
    url = mkTalkUrl slug
    kv = kvConn config

saveToDB :: Config -> Text -> IO (Maybe Talk)
saveToDB config@Config{..} url = do
  mTalk <- fetchTalk httpManager url
  case mTalk of
    Just talk -> do
      void $ KV.runRedis kvConn $ KV.multiExec $ do
          void $ KV.setex (Keys.cache $ _talkId talk) (3600*24) ""
          KV.set (Keys.slug $ _talkSlug talk) (C.pack $ show $ _talkId talk)

      runBeamPostgres dbConn $ runInsert $
        Pg.insert (_talks talkDb) (insertValues [ talk ]) $
          Pg.onConflict (Pg.conflictingFields primaryKey) $
            Pg.onConflictUpdateInstead $
              \t -> ( _talkLanguages t
                    , _talkMediaPad t
                    )

      saveTranscriptIfNotAlready config talk
      return $ Just talk
    Nothing -> return Nothing

saveTranscriptIfNotAlready :: Config -> Talk -> IO ()
saveTranscriptIfNotAlready config Talk {..} = do
  xs <- runBeamPostgres conn $ runSelectReturningOne $ select
    ( filter_ (\transcript -> (_transcriptTalk transcript ==. val_ (TalkId _talkId)))
    $ all_ (_transcripts talkDb)
    )
  case xs of
    Just _ -> pure ()
    Nothing   -> do
      path <- toSub $
        Subtitle _talkId _talkSlug ["en"] _talkMediaSlug _talkMediaPad TXT
      case path of
        Just path' -> do
          transcript <- T.drop 2 <$> T.readFile path'
          -- runBeamPostgres conn $ runInsert $
          --   Pg.insert (_transcripts talkDb)
          --     ( insertExpressions
          --       [ Transcript (val_ $ TalkId _talkId) $ toTsVector (Just english) (val_ transcript) ]
          --     ) $
          --     Pg.onConflict Pg.anyConflict Pg.onConflictDoNothing
          void $ DB.execute conn [sql|
              INSERT INTO transcripts (id, en_tsvector)
              VALUES (?, to_tsvector('english', ? || ?))
          |] (_talkId, _talkName, transcript)
        Nothing -> return ()
  where
    conn = dbConn config

fetchTalk :: Manager -> Text -> IO (Maybe Talk)
fetchTalk manager url = do
  handle (\(_::HttpException) -> return Nothing) $ do
    req <- parseUrlThrow $ T.unpack url
    res <- httpLbs req manager
    let
      body = responseBody res
      cursor = fromDocument $ parseLBS body
      desc = parseDescription cursor
      img = parseImage cursor
      mdSlug = parseMediaSlug body
      mdPad = parseMediaPad body
      core = parseTalkObject body
    case decode core of
      Just TalkObj{..} -> do
        return $ Just Talk
          { _talkId = id
          , _talkName = name
          , _talkSlug = slug
          , _talkFilmed = filmedAt
          , _talkPublished = publishedAt
          , _talkDescription = desc
          , _talkImage = img
          , _talkLanguages = languages
          , _talkMediaSlug = mdSlug
          , _talkMediaPad = mdPad
          }
      _      -> do
        print $ "<fetchTalk>: parse error " <> T.unpack url
        pure Nothing

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
