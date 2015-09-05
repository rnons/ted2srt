{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module ReTed.API
  ( tedApi
  , tedServer
  ) where

import           Control.Applicative((<$>), (<*>))
import           Control.Monad (forM, liftM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT, left)
import           Data.Aeson (encode, decodeStrict)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import           Data.Maybe (catMaybes, mapMaybe, fromJust, fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Database.Redis hiding (decode)
import qualified Filesystem.Path.CurrentOS as FS
import           Network.HTTP.Types (status200, status404)
import           Network.Wai (Application, Response, responseFile, responseLBS)
import           Servant
import           System.Random (randomRIO)

import Web.TED (FileType(..), Subtitle(..), getTalkId, queryTalk, toSub)
import qualified Web.TED as API
import ReTed.Types

instance FromText FileType where
    fromText "srt" = Just SRT
    fromText "vtt" = Just VTT
    fromText "txt" = Just TXT
    fromText "lrc" = Just LRC
    fromText _     = Nothing

type TedApi =
       "talks" :> QueryParam "tid" Integer :> QueryParam "limit" Integer :> Get '[JSON] [RedisTalk]
  :<|> "talks" :> "random" :> Get '[JSON] RedisTalk
  :<|> "talks" :> Capture "slug" Text :> Get '[JSON] TalkResp
  :<|> "talks" :> Capture "tid" Int :> "transcripts" :> Capture "format" FileType :> QueryParams "lang" Text :> Raw
  :<|> "talks" :> Capture "tid" Int :> "transcripts" :> "download" :> Capture "format" FileType :> QueryParams "lang" Text :> Raw
  :<|> "search" :> QueryParam "q" Text :> Get '[JSON] [RedisTalk]

type Handler t = EitherT ServantErr IO t

notFound :: (Response -> t) -> t
notFound respond = respond $ responseLBS status404 [] "Not Found"

getTalksH :: Connection -> Maybe Integer -> Maybe Integer -> Handler [RedisTalk]
getTalksH conn mStartTid mLimit = do
    emtalks <- liftIO $ runRedis conn $ do
        emrank <- zrevrank "tids" (C.pack $ show startTid)
        let start = maybe 0 (+ 1) (either (const Nothing) Prelude.id emrank)
        elatest <- zrevrange "tids" start (start + limit - 1)
        mget $ either (const []) Prelude.id elatest
    let talks = mapMaybe decodeStrict $ catMaybes $
                    either (const [Nothing]) Prelude.id emtalks
    return talks
  where
    defaultLimit = 10
    startTid = fromMaybe 0 mStartTid
    limit' = fromMaybe defaultLimit mLimit
    limit = if limit' > defaultLimit then defaultLimit else limit'

getTalkH :: Connection -> Text -> Handler TalkResp
getTalkH conn slug = do
    emtid <- liftIO $ runRedis conn $ get $ C.pack $ T.unpack slug

    case emtid of
        Right (Just tid) -> do
            result <- liftIO $ runRedis conn $ multiExec $ do
                t <- get tid
                c <- get ("cache:" <> tid)
                return $ (,) <$> t <*> c
            case result of
                TxSuccess (Just talk, Just cache) ->
                    let retTalk = fromJust $ decodeStrict talk
                        retCache = fromJust $ decodeStrict cache
                    in return $ TalkResp retTalk retCache
                TxError _ -> left err404
                _ -> do
                    talk' <- liftIO $ queryTalk $ read $ C.unpack tid
                    case talk' of
                        Just talk -> do
                            let cache = tedTalkToCache talk
                            liftIO $ runRedis conn $
                                setex ("cache:" <> tid) (3600*24)
                                      (L.toStrict $ encode cache)
                            getTalkH conn slug
                        Nothing   -> left err404
        Right Nothing    -> do
            mtid <- liftIO $ getTalkId $ mkTalkUrl slug
            case mtid of
                Just tid -> do
                    talk' <- liftIO $ queryTalk tid
                    case talk' of
                        Nothing   -> left err404
                        Just talk -> do
                            dbtalk <- liftIO $ marshal talk
                            let cache = tedTalkToCache talk
                            liftIO $ runRedis conn $ multiExec $ do
                                set (C.pack $ T.unpack $ API.slug talk)
                                    (C.pack $ show tid)
                                set (C.pack $ show tid)
                                    (L.toStrict $ encode dbtalk)
                                setex ("cache:" <> C.pack (show tid))
                                      (3600*24)
                                      (L.toStrict $ encode cache)
                            return $ TalkResp dbtalk cache
                _         -> left err404
        Left _ -> left err404

getSubtitlePath :: Connection -> Int -> FileType -> [Text] -> IO (Maybe FilePath)
getSubtitlePath conn tid format lang = do
    talk <- getTalkFromRedis conn tid
    toSub $ Subtitle tid (slug talk) lang (mSlug talk) (mPad talk) format

getTalkSubtitleH :: Connection -> Int -> FileType -> [Text] -> Application
getTalkSubtitleH conn tid format lang _ respond = do
    let cType = if format == VTT then "text/vtt" else "text/plain"
    path <- liftIO $ getSubtitlePath conn tid format lang
    case path of
        Just p  -> respond $ responseFile status200 [("Content-Type", cType)] p Nothing
        Nothing -> notFound respond

downloadTalkSubtitleH :: Connection
                      -> Int
                      -> FileType
                      -> [Text]
                      -> Application
downloadTalkSubtitleH conn tid format lang _ respond = do
    path <- liftIO $ getSubtitlePath conn tid format lang
    case path of
        Just p  -> do
            let filename = C.pack $ FS.encodeString $ FS.filename $ FS.decodeString p
            respond $ responseFile
                status200
                [ ("Content-Type", "text/plain")
                , ("Content-Disposition", "attachment; filename=" <> filename)]
                p
                Nothing
        Nothing -> notFound respond

getSearchH :: Connection -> Maybe Text -> Handler [RedisTalk]
getSearchH conn (Just q) = liftIO $ do
    searchtalks <- API.searchTalk q
    liftM catMaybes $ forM searchtalks $ \t -> do
        mtalk <- runRedis conn $ get (C.pack $ show $ API.s_id t)
        case mtalk of
            Right (Just talk') -> return $ decodeStrict talk'
            _                    -> do
                talk' <- queryTalk $ API.s_id t
                case talk' of
                    Nothing -> return Nothing
                    Just talk -> do
                        dbtalk <- marshal talk
                        runRedis conn $ multiExec $ do
                            set (C.pack $ show $ API.s_id t)
                                (L.toStrict $ encode dbtalk)
                            zadd "tids" [(realToFrac $ utcTimeToPOSIXSeconds $ publishedAt dbtalk, C.pack $ show $ API.s_id t)]
                        return $ Just dbtalk
getSearchH _ Nothing = left err400

getRandomTalkH :: Connection -> Handler RedisTalk
getRandomTalkH conn = liftIO $ do
    mCount <- runRedis conn $ zcard "tids"
    case mCount of
        Right count -> do
            r <- randomRIO (0, count-1)
            mTid <- runRedis conn $ zrange "tids" r r
            getTalkFromRedis conn $ either (const 0) (read . C.unpack . head) mTid
        Left err -> error $ show err

getTalkFromRedis :: Connection -> Int -> IO RedisTalk
getTalkFromRedis conn tid = do
    result <- runRedis conn $ get (C.pack $ show tid)
    case result of
        Right (Just talk') -> do
            return $ fromJust $ decodeStrict talk'
        Right Nothing -> error "nothing"
        Left err -> error $ show err

tedApi :: Proxy TedApi
tedApi = Proxy

tedServer :: Connection -> Server TedApi
tedServer conn =
        getTalksH conn
   :<|> getRandomTalkH conn
   :<|> getTalkH conn
   :<|> getTalkSubtitleH conn
   :<|> downloadTalkSubtitleH conn
   :<|> getSearchH conn
