module View.Talk where

import           Lucid
import           RIO
import           Servant     (err404)
import           Types
import           View.Bundle
import           View.Error

getTalkH :: Text -> AppM (Html ())
getTalkH slug = do
  get404H
  bundle <- includeBundle TalkBundle
  pure $ doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport"
            ,content_ "width=device-width, initial-scale=1"]
    body_ $ do
      bundle
