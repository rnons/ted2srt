module View.Talk where

import           Lucid
import           RIO
import           Types
import           View.Bundle

getTalkH :: Text -> AppM (Html ())
getTalkH slug = do
  bundle <- includeBundle TalkBundle
  pure $ doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport"
            ,content_ "width=device-width, initial-scale=1"]
    body_ $ do
      bundle
