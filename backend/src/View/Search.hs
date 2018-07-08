module View.Search where

import qualified Data.Text   as T
import           Lucid
import           RIO
import           Types
import           View.Bundle
import           View.Error
import           View.Layout (layout)

getSearchH :: Maybe Text -> AppM (Html ())
getSearchH mq =
  case mq of
    Nothing -> get404H
    Just q -> do
      bundle <- includeBundle SearchBundle
      layout
        ( do
          title_ $ toHtml $ q <> " - TED2srt search"
          script_ $ "window.Q = " <> T.pack (show q)
        )
        bundle
