{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import           Control.Monad (forM_, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import           Data.Maybe (catMaybes, mapMaybe, fromJust)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time (getCurrentTime)
import           Database.Redis ( connect, defaultConnectInfo, runRedis
                                , multiExec, get, set, del, mget, rpush)
import           Network.HTTP.Conduit (simpleHttp)
import           Prelude
import           Text.HTML.DOM (parseLBS)
import qualified Text.XML as X
import           Text.XML.Cursor

import Handler.Util (RedisTalk(..), marshal)
import Web.TED hiding (content)


type TalkId = Int

main :: IO ()
main = do
    res <- simpleHttp rurl
    let cursor = fromDocument $ parseLBS res
        tids = take limit (parseTids cursor)

    saveToRedis tids
    X.writeFile X.def "static/atom.xml" . template =<< mkFeed =<< saveAsFeed tids
  where
    limit = 5
    rurl = "http://feeds.feedburner.com/tedtalks_video"
    -- 105 tids
    parseTids :: Cursor -> [TalkId]
    parseTids cur = map (read . T.unpack) $ cur $// element "jwplayer:talkId"
                                                &// content

saveToRedis :: [TalkId] -> IO ()
saveToRedis tids = do
    conn <- connect defaultConnectInfo
    runRedis conn $ multiExec $
        del [key] >> rpush key (map (C.pack . show) tids)
    forM_ tids $ \tid -> do
        mtalk <- runRedis conn $ get (C.pack $ show tid)
        case mtalk of
            Right (Just _) -> return ()
            Right Nothing  -> do
                talk' <- liftIO $ queryTalk tid
                case talk' of
                    Nothing -> return ()
                    Just talk -> do
                        dbtalk <- liftIO $ marshal talk
                        void $ runRedis conn $ multiExec $ do
                            set (C.pack $ T.unpack $ Web.TED.slug talk)
                                (C.pack $ show tid)
                            set (C.pack $ show tid)
                                (L.toStrict $ encode dbtalk)
            Left err        -> error $ show err
  where
    key = "latest"

talkToFeedEntry :: RedisTalk -> IO (Maybe FeedEntry)
talkToFeedEntry RedisTalk {..} = do
    path <- toSub $
        Subtitle "" slug ["en"] mSlug mPad TXT
    case path of
        Just path' -> do
          transcript <- T.readFile path'
          return $ Just FeedEntry
              { feedEntryTitle = name
              , feedEntryLink  = "http://ted2srt.org/talks/" <> slug
              , feedEntryUpdated = fromJust publishedAt
              , feedEntryContent = ppr transcript
              }
        Nothing -> return Nothing
  where
    ppr txt = T.concat $ map (\p -> "<p>" <> p <> "</p>") (T.lines txt)

saveAsFeed :: [TalkId] -> IO [FeedEntry]
saveAsFeed tids = do
    conn <- connect defaultConnectInfo
    emtalks <- runRedis conn $ mget $ map (C.pack . show) tids
    let talks = mapMaybe decodeStrict $ catMaybes $
                either (const [Nothing]) Prelude.id emtalks
    return . catMaybes =<< mapM talkToFeedEntry talks

mkFeed :: [FeedEntry] -> IO Feed
mkFeed entries = do
    time <- getCurrentTime
    return Feed
        { feedTitle = "TED2srt"
        , feedLinkSelf = "http://ted2srt.org/static/atom.xml"
        , feedLinkHome = "http://ted2srt.org"
        , feedAuthor = "rnons"
        , feedUpdated = time
        , feedEntries = entries
        }
