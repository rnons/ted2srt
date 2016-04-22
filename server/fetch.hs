{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import           Control.Monad (void, zipWithM)
import           Data.Aeson (decodeStrict)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time (getCurrentTime)
import           Database.Redis ( connect, defaultConnectInfo, runRedis
                                , mget, connectDatabase)
import           LoadEnv (loadEnv)
import           Network.HTTP.Conduit (simpleHttp)
import           Prelude hiding (id)
import qualified Prelude
import           Text.HTML.DOM (parseLBS)
import           Text.XML.Cursor

import           ReTed.Config (getConfig)
import qualified ReTed.Models.Talk as Talk
import           ReTed.Types (RedisTalk(..))
import           Web.TED hiding (content, publishedAt)


type TalkId = Int

main :: IO ()
main = do
    loadEnv
    res <- simpleHttp rurl
    config <- getConfig
    let cursor = fromDocument $ parseLBS res
        tids = take limit (parseTids cursor)
        urls = take limit (parseUrl cursor)

    void $ mapM (Talk.saveToDB config) urls
    -- X.writeFile X.def "atom.xml" . template =<< mkFeed =<< saveAsFeed tids
  where
    limit = 5
    rurl = "http://feeds.feedburner.com/tedtalks_video"
    -- 105 tids
    parseTids :: Cursor -> [TalkId]
    parseTids cur = map (read . T.unpack) $ cur $// element "jwplayer:talkId"
                                                &// content
    parseUrl :: Cursor -> [Text]
    parseUrl cur = cur $// element "feedburner:origLink"
                       &// content


talkToFeedEntry :: RedisTalk -> IO (Maybe FeedEntry)
talkToFeedEntry RedisTalk {..} = do
    path <- toSub $
        Subtitle 0 slug ["en"] mSlug mPad TXT
    case path of
        Just path' -> do
          transcript <- T.readFile path'
          return $ Just FeedEntry
              { feedEntryTitle = name
              , feedEntryLink  = "http://ted2srt.org/talks/" <> slug
              , feedEntryUpdated = publishedAt
              , feedEntryContent = ppr transcript
              }
        Nothing -> return Nothing
  where
    ppr txt = T.concat $ map (\p -> "<p>" <> p <> "</p>") (T.lines txt)

saveAsFeed :: [TalkId] -> IO [FeedEntry]
saveAsFeed tids = do
    conn <- connect defaultConnectInfo { connectDatabase = 1 }
    emtalks <- runRedis conn $ mget $ map (C.pack . show) tids
    let talks = mapMaybe decodeStrict $ catMaybes $
                either (const [Nothing]) Prelude.id emtalks
    return . catMaybes =<< mapM talkToFeedEntry talks

mkFeed :: [FeedEntry] -> IO Feed
mkFeed entries = do
    time <- getCurrentTime
    return Feed
        { feedTitle = "TED2srt"
        , feedLinkSelf = "http://ted2srt.org/atom.xml"
        , feedLinkHome = "http://ted2srt.org"
        , feedAuthor = "rnons"
        , feedUpdated = time
        , feedEntries = entries
        }
