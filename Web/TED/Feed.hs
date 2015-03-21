-- Mostly taken from yesod-newsfeed
-- https://hackage.haskell.org/package/yesod-newsfeed
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.TED.Feed
  ( Feed (..)
  , FeedEntry (..)
  , template
  ) where

import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, formatTime, defaultTimeLocale)
import           Text.XML


data Feed = Feed
    { feedTitle    :: Text
    , feedLinkSelf :: Text
    , feedLinkHome :: Text
    , feedAuthor   :: Text
    , feedUpdated  :: UTCTime
    , feedEntries  :: [FeedEntry]
    }

data FeedEntry = FeedEntry
    { feedEntryTitle      :: Text
    , feedEntryLink       :: Text
    , feedEntryUpdated    :: UTCTime
    , feedEntryContent    :: Text
    }

-- | Format a 'UTCTime' in W3 format.
formatW3 :: UTCTime -> T.Text
formatW3 = T.pack . formatTime defaultTimeLocale "%FT%X-00:00"

template :: Feed -> Document
template Feed {..} =
    Document (Prologue [] Nothing []) (addNS root) []
  where
    addNS (Element (Name ln _ _) as ns) = Element (Name ln (Just namespace) Nothing) as (map addNS' ns)
    addNS' (NodeElement e) = NodeElement $ addNS e
    addNS' n = n
    namespace = "http://www.w3.org/2005/Atom"

    root = Element "feed" Map.empty $ map NodeElement
        $ Element "title" Map.empty [NodeContent feedTitle]
        : Element "link" (Map.fromList [("rel", "self"), ("href", feedLinkSelf)]) []
        : Element "link" (Map.singleton "href" feedLinkHome) []
        : Element "updated" Map.empty [NodeContent $ formatW3 feedUpdated]
        : Element "id" Map.empty [NodeContent feedLinkHome]
        : Element "author" Map.empty [NodeElement $ Element "name" Map.empty [NodeContent feedAuthor]]
        : map entryTemplate feedEntries

entryTemplate :: FeedEntry -> Element
entryTemplate FeedEntry {..} = Element "entry" Map.empty $ map NodeElement
    [ Element "id" Map.empty [NodeContent feedEntryLink]
    , Element "link" (Map.singleton "href" feedEntryLink) []
    , Element "updated" Map.empty [NodeContent $ formatW3 feedEntryUpdated]
    , Element "title" Map.empty [NodeContent feedEntryTitle]
    , Element "content" (Map.singleton "type" "html") [NodeContent feedEntryContent]
    ]
