module View.Search where

import qualified Data.Text   as T
import           Lucid
import           RIO
import           Types
import           View.Bundle
import           View.Error

getSearchH :: Maybe Text -> AppM (Html ())
getSearchH mq =
  case mq of
    Nothing -> get404H
    Just q -> do
      bundle <- includeBundle SearchBundle
      pure $ doctypehtml_ $ do
        head_ $ do
          meta_ [charset_ "utf-8"]
          meta_ [name_ "viewport"
                ,content_ "width=device-width, initial-scale=1"]
          script_ $
            "window.Q = " <> T.pack (show q)
        body_ $ do
          bundle
