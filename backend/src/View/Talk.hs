module View.Talk where

import           Data.Aeson      (encode)
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy  as LT
import           Lucid
import           Models.Talk     (getTalkBySlug)
import           RIO
import           Types
import           View.Bundle
import           View.Error

getTalkH :: Text -> AppM (Html ())
getTalkH slug = do
  mTalk <- lift $ getTalkBySlug slug
  case mTalk of
    Nothing   -> get404H
    Just talk -> do
      bundle <- includeBundle TalkBundle
      pure $ doctypehtml_ $ do
        head_ $ do
          meta_ [charset_ "utf-8"]
          meta_ [name_ "viewport"
                ,content_ "width=device-width, initial-scale=1"]
          script_ $ LT.toStrict $
            "window.TALK = " <> encodeToLazyText talk
        body_ $ do
          bundle
