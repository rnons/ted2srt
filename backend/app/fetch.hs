{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import qualified Data.Text            as T
import qualified Data.Text.IO.Utf8    as Utf8
import           Data.Time            (getCurrentTime)
import           Database.Persist     (Entity (..))
import           LoadEnv              (loadEnv)
import           Network.HTTP.Conduit (simpleHttp)
import           RIO
import           Text.HTML.DOM        (parseLBS)
import qualified Text.XML             as X
import           Text.XML.Cursor

import           Config               (Config (..), mkConfig)
import           Models.Talk          (getTalks, saveToDB)
import           Types
import           Web.TED              (Feed (..), FeedEntry (..), FileType (..),
                                       Subtitle (..), template, toSub)

main :: IO ()
main = do
    loadEnv
    res <- simpleHttp rurl
    config <- mkConfig
    let cursor = fromDocument $ parseLBS res
        urls = take limit (parseUrl cursor)

    void $ runRIO config $ mapM saveToDB urls
    X.writeFile X.def "atom.xml" . template =<< mkFeed
                                            =<< saveAsFeed config
  where
    limit = 5
    rurl = "http://feeds.feedburner.com/tedtalks_video"
    -- 105 tids
    parseUrl :: Cursor -> [Text]
    parseUrl cur = cur $// element "feedburner:origLink"
                       &// content


talkToFeedEntry :: Entity Talk -> IO (Maybe FeedEntry)
talkToFeedEntry (Entity _ Talk {..}) = do
    path <- toSub $
        Subtitle 0 talkSlug ["en"] talkMediaSlug talkMediaPad TXT
    case path of
        Just path' -> do
          transcript <- T.drop 2 <$> Utf8.readFile path'
          return $ Just FeedEntry
              { feedEntryTitle = talkName
              , feedEntryLink  = "http://ted2srt.org/talks/" <> talkSlug
              , feedEntryUpdated = talkPublishedAt
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
