{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative((<$>), (<*>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (left)
import           Data.Aeson (encode, decodeStrict)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import           Data.Maybe (catMaybes, mapMaybe, fromJust, fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Database.Redis hiding (decode)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant

import Web.TED (FileType(..), Subtitle(..), getTalkId, queryTalk, toSub)
import qualified Web.TED as API
import Handler.Util

instance FromText FileType where
    fromText "srt" = Just SRT
    fromText "vtt" = Just VTT
    fromText "txt" = Just TXT
    fromText "lrc" = Just LRC
    fromText _     = Nothing

type TedApi =
       "newest" :> Get '[JSON] [RedisTalk]
  :<|> "talks" :> Capture "slug" Text :> Get '[JSON] RedisTalk
  :<|> "talks" :> Capture "tid" Int :> "subtitles" :> Capture "format" FileType :> QueryParam "lang" Text :> Get '[JSON] Text

getNewstH conn = do
    emtalks <- liftIO $ runRedis conn $ do
        elatest <- lrange "latest" 0 4
        mget $ either (const []) Prelude.id elatest
    let talks = mapMaybe decodeStrict $ catMaybes $
                    either (const [Nothing]) Prelude.id emtalks
    return talks

getTalkH conn slug = do
    emtid <- liftIO $ runRedis conn $ get $ C8.pack $ T.unpack slug

    case emtid of
        Right (Just tid) -> do
            result <- liftIO $ runRedis conn $ multiExec $ do
                t <- get tid
                c <- get ("cache:" <> tid)
                return $ (,) <$> t <*> c
            case result of
                TxSuccess (Just talk, Just cache) ->
                    return (fromJust $ decodeStrict talk)
                           -- (fromJust $ decodeStrict cache)
                TxError _ -> left (404, "not found")
                _ -> do
                    talk' <- liftIO $ queryTalk $ read $ C8.unpack tid
                    case talk' of
                        Just talk -> do
                            let value = apiTalkToValue talk
                            liftIO $ runRedis conn $
                                setex ("cache:" <> tid) (3600*24)
                                      (L.toStrict $ encode value)
                            getTalkH conn slug
                        Nothing   -> left (404, "not found")
        Right Nothing    -> do
            mtid <- liftIO $ getTalkId $ talkUrl <> slug
            case mtid of
                Just tid -> do
                    talk' <- liftIO $ queryTalk tid
                    case talk' of
                        Nothing   -> left (404, "not found")
                        Just talk -> do
                            dbtalk <- liftIO $ marshal talk
                            let value = apiTalkToValue talk
                            liftIO $ runRedis conn $ multiExec $ do
                                set (C8.pack $ T.unpack $ API.slug talk)
                                    (C8.pack $ show tid)
                                set (C8.pack $ show tid)
                                    (L.toStrict $ encode dbtalk)
                                setex ("cache:" <> C8.pack (show tid))
                                      (3600*24)
                                      (L.toStrict $ encode value)
                            return dbtalk
                _         -> left (404, "not found")
        Left _ -> left (404, "not found")

getTalkSubtitleH conn tid format lang = do
    path <- liftIO $ do
        talk <- getTalkFromRedis conn tid
        toSub $ Subtitle tid (slug talk) [fromMaybe "en" lang] (mSlug talk) (mPad talk) format
    case path of
        Just p -> do
            text <- liftIO $ T.readFile p
            return text
        _      -> left (404, "not found")

getTalkFromRedis :: Connection -> Int -> IO RedisTalk
getTalkFromRedis conn tid = do
    -- (Right (Just talk')) <- runRedis conn $
    result <- runRedis conn $ get (C8.pack $ show tid)
    case result of
        Right (Just talk') -> do
            print talk'
            return $ fromJust $ decodeStrict talk'
        Right Nothing -> error "nothing"
        Left err -> error $ show err

tedApi :: Proxy TedApi
tedApi = Proxy

tedServer :: Connection -> Server TedApi
tedServer conn = getNewstH conn :<|> getTalkH conn :<|> getTalkSubtitleH conn

app :: Connection -> Application
app = serve tedApi . tedServer

main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    run 3001 $ app conn
