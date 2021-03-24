module Models.Talk where

import           Control.Monad               (liftM, mzero, void)
import           Data.Aeson
import qualified Data.ByteString.Char8       as C
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Read              as T
import           Data.Time                   (UTCTime)
import           Data.Time.Clock.POSIX       (posixSecondsToUTCTime)
import           Database.Persist
import qualified Database.Redis              as KV
import           GHC.Generics                (Generic)
import           Network.HTTP.Client.Conduit (HttpException, httpLbs,
                                              parseUrlThrow, responseBody)
import           RIO                         hiding (id)
import           Text.HTML.DOM               (parseLBS)
import           Text.XML.Cursor             (fromDocument)
import           Types                       (AppRIO, runDB)

import           Config                      (Config (..))
import           Model
import qualified Models.RedisKeys            as Keys
import           Models.Types                (mkTalkUrl)
import           Web.TED.TalkPage            (parseDescription, parseImage,
                                              parseTalkObject)


data TalkObj = TalkObj
  { id          :: Int
  , name        :: Text
  , slug        :: Text
  , filmedAt    :: UTCTime
  , publishedAt :: UTCTime
  , languages   :: [Language]
  , mediaSlug   :: Text
  } deriving (Generic)

instance FromJSON TalkObj where
  parseJSON (Object v) = do
    idText <- v .: "id"
    tid <- case fst <$> T.decimal idText of
      Right tid -> pure tid
      _         -> fail "id is not int"
    TalkObj
      <$> pure tid
      <*> v .: "name"
      <*> v .: "slug"
      <*> liftM posixSecondsToUTCTime (v .: "published")
      <*> liftM posixSecondsToUTCTime (v .: "published")
      <*> v .: "languages"
      <*> v .: "mediaIdentifier"
  parseJSON _          = mzero


getTalks :: Int -> Int -> AppRIO [Entity Talk]
getTalks offset limit = do
  runDB $ selectList []
    [ Desc TalkId
    , LimitTo limit
    , OffsetBy offset
    ]

getTalk :: Int -> Text -> AppRIO (Maybe (Entity Talk))
getTalk tid url = do
  Config { kvConn } <- ask
  cached <- liftIO $ KV.runRedis kvConn $
    KV.get $ Keys.cache $ fromIntegral tid
  case cached of
    Right (Just _) -> getTalkById tid (Just url)
    Right Nothing  -> saveToDB url
    Left _         -> saveToDB url

getTalkById :: Int -> Maybe Text -> AppRIO (Maybe (Entity Talk))
getTalkById tid mUrl = do
  xs <- runDB $ getEntity $ TalkKey tid
  case xs of
    Just talk -> return $ Just talk
    _         -> maybe (return Nothing) saveToDB mUrl

hush :: Either a b -> Maybe b
hush (Left _)  = Nothing
hush (Right v) = Just v

getTalkBySlug :: Text -> AppRIO (Maybe (Entity Talk))
getTalkBySlug slug = do
  Config { kvConn } <- ask
  mtid <- liftIO $ fmap (join . hush) <$> KV.runRedis kvConn $ KV.get $ Keys.slug slug
  case mtid of
    Just tid ->
      case readMaybe $ C.unpack tid of
        Just tid' -> getTalk tid' url
        Nothing   -> pure Nothing
    Nothing  ->
      saveToDB url
  where
    url = mkTalkUrl slug

saveToDB :: Text -> AppRIO (Maybe (Entity Talk))
saveToDB url = do
  Config{..} <- ask
  mTalk <- fetchTalk url
  case mTalk of
    Just entity@(Entity talkId talk) -> do
      void $ liftIO $ KV.runRedis kvConn $ KV.multiExec $ do
          void $ KV.setex (Keys.cache $ unTalkKey talkId) (3600*24) ""
          KV.set (Keys.slug $ talkSlug talk) (C.pack $ show $ unTalkKey talkId)

      runDB $ repsert talkId talk
      return $ Just entity
    Nothing -> return Nothing

fetchTalk :: Text -> AppRIO (Maybe (Entity Talk))
fetchTalk url = do
  handle (\(_::HttpException) -> return Nothing) $ do
    req <- parseUrlThrow $ T.unpack url
    res <- httpLbs req
    let
      body = responseBody res
      cursor = fromDocument $ parseLBS body
      desc = parseDescription cursor
      img = parseImage cursor
      core = parseTalkObject body
    case eitherDecode core of
      Right TalkObj{..} -> do
        return $ Just $ Entity (TalkKey $ fromIntegral id) (Talk
          { talkName = name
          , talkSlug = slug
          , talkFilmedAt = filmedAt
          , talkPublishedAt = publishedAt
          , talkDescription = desc
          , talkImage = img
          , talkLanguages = toJSON languages
          , talkMediaSlug = mediaSlug
          , talkMediaPad = 0.0
          })
      Left err      -> do
        logErrorS "fetchTalk" $ fromString err
        pure Nothing
