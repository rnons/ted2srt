{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module ReTed.API
  ( tedApi
  , tedServer
  ) where

import           Control.Monad.IO.Class    (liftIO)
import qualified Data.ByteString.Char8     as C
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Filesystem.Path.CurrentOS as FS
import           Network.HTTP.Types        (status200, status404)
import           Network.Wai               (Application, Response, responseFile,
                                            responseLBS)
import           Servant

import           ReTed.Config              (Config (..))
import           ReTed.Models.Talk         (Talk (..), getRandomTalk,
                                            getTalkById, getTalkBySlug,
                                            getTalks, searchTalk)
import           Web.TED                   (FileType (..), Subtitle (..), toSub)


instance FromHttpApiData FileType where
    parseUrlPiece "srt" = Right SRT
    parseUrlPiece "vtt" = Right VTT
    parseUrlPiece "txt" = Right TXT
    parseUrlPiece "lrc" = Right LRC
    parseUrlPiece _     = Left "Unsupported"

type TedApi =
       "talks" :> QueryParam "tid" Int          -- ^ getTalksH
               :> QueryParam "limit" Int
               :> Get '[JSON] [Talk]
  :<|> "talks" :> "random"
               :> Get '[JSON] Talk
  :<|> "talks" :> Capture "slug" Text           -- ^ getTalkH
               :> Get '[JSON] Talk
  :<|> "talks" :> Capture "tid" Int             -- ^ getTalkSubtitleH
               :> "transcripts"
               :> Capture "format" FileType
               :> QueryParams "lang" Text
               :> Raw
  :<|> "talks" :> Capture "tid" Int             -- ^ downloadTalkSubtitleH
               :> "transcripts"
               :> "download"
               :> Capture "format" FileType
               :> QueryParams "lang" Text
               :> Raw
  :<|> "search" :> QueryParam "q" Text :> Get '[JSON] [Talk]

notFound :: (Response -> t) -> t
notFound respond = respond $ responseLBS status404 [] "Not Found"

getTalksH :: Config -> Maybe Int -> Maybe Int -> Handler [Talk]
getTalksH config mStartTid mLimit = do
    talks <- liftIO $ getTalks conn limit
    return talks
  where
    conn = dbConn config
    defaultLimit = 10
    startTid = fromMaybe 0 mStartTid
    limit' = fromMaybe defaultLimit mLimit
    limit = if limit' > defaultLimit then defaultLimit else limit'

getTalkH :: Config -> Text -> Handler Talk
getTalkH config slug = do
    mTalk <- liftIO $ getTalkBySlug config slug
    case mTalk of
        Just talk -> return talk
        Nothing   -> throwError err404

getSubtitlePath :: Config -> Int -> FileType -> [Text] -> IO (Maybe FilePath)
getSubtitlePath config tid format lang = do
    mTalk <- getTalkById config tid Nothing
    case mTalk of
        Just talk -> toSub $
            Subtitle tid (slug talk) lang (mediaSlug talk) (mediaPad talk) format
        Nothing -> return Nothing

getTalkSubtitleH :: Config -> Int -> FileType -> [Text] -> Application
getTalkSubtitleH config tid format lang _ respond = do
    let cType = if format == VTT then "text/vtt" else "text/plain"
    path <- liftIO $ getSubtitlePath config tid format lang
    case path of
        Just p  -> respond $ responseFile status200 [("Content-Type", cType)] p Nothing
        Nothing -> notFound respond

downloadTalkSubtitleH :: Config
                      -> Int
                      -> FileType
                      -> [Text]
                      -> Application
downloadTalkSubtitleH config tid format lang _ respond = do
    path <- liftIO $ getSubtitlePath config tid format lang
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

getSearchH :: Config -> Maybe Text -> Handler [Talk]
getSearchH config (Just q) = liftIO $ searchTalk config q
getSearchH _ Nothing       = throwError err400

getRandomTalkH :: Config -> Handler Talk
getRandomTalkH config = do
    mTalk <- liftIO $ getRandomTalk config
    maybe (throwError err404) return mTalk

tedApi :: Proxy TedApi
tedApi = Proxy

tedServer :: Config -> Server TedApi
tedServer config =
         getTalksH config
    :<|> getRandomTalkH config
    :<|> getTalkH config
    :<|> (\tid format lang -> Tagged (getTalkSubtitleH config tid format lang))
    :<|> (\tid format lang -> Tagged (downloadTalkSubtitleH config tid format lang))
    :<|> getSearchH config
