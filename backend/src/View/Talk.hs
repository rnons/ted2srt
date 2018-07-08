module View.Talk where

import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy  as LT
import           Lucid
import           Models.Talk     (getTalkBySlug)
import           RIO
import           Types
import           View.Bundle
import           View.Error
import           View.Layout     (layout)

getTalkH :: Text -> AppM (Html ())
getTalkH slug = do
  mTalk <- lift $ getTalkBySlug slug
  case mTalk of
    Nothing   -> get404H
    Just talk -> do
      bundle <- includeBundle TalkBundle
      pure $ layout
        ( do
          title_ $ toHtml $ _talkName talk
          script_ $ LT.toStrict $
            "window.TALK = " <> encodeToLazyText talk
        )
        bundle
