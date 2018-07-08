{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Time                  (getCurrentTime)
import qualified Database.PostgreSQL.Simple as DB
import           LoadEnv                    (loadEnv)
import           Network.HTTP.Conduit       (simpleHttp)
import           RIO
import           Text.HTML.DOM              (parseLBS)
import qualified Text.XML                   as X
import           Text.XML.Cursor

import           Config                     (Config (..), getConfig)
import           Models.Talk                (getTalks, saveToDB)
import           Types
import           Web.TED                    (Feed (..), FeedEntry (..),
                                             FileType (..), Subtitle (..),
                                             template, toSub)

type TalkId = Int

main :: IO ()
main = do
    loadEnv
    res <- simpleHttp rurl
    config <- getConfig
    let cursor = fromDocument $ parseLBS res
        tids = take limit (parseTids cursor)
        urls = take limit (parseUrl cursor)

    runRIO config $ mapM saveToDB urls
    X.writeFile X.def "atom.xml" . template =<< mkFeed
                                            =<< saveAsFeed config
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


talkToFeedEntry :: Talk -> IO (Maybe FeedEntry)
talkToFeedEntry Talk {..} = do
    path <- toSub $
        Subtitle 0 _talkSlug ["en"] _talkMediaSlug _talkMediaPad TXT
    case path of
        Just path' -> do
          transcript <- T.drop 2 <$> T.readFile path'
          return $ Just FeedEntry
              { feedEntryTitle = _talkName
              , feedEntryLink  = "http://ted2srt.org/talks/" <> _talkSlug
              , feedEntryUpdated = _talkPublishedAt
              , feedEntryContent = ppr transcript
              }
        Nothing -> return Nothing
  where
    ppr txt = T.concat $ map (\p -> "<p>" <> p <> "</p>") (T.lines txt)

saveAsFeed :: Config -> IO [FeedEntry]
saveAsFeed config = do
    talks <- runRIO config $ getTalks 0 5
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
