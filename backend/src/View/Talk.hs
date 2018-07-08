module View.Talk where

import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy  as LT
import           Lucid
import           Lucid.Base      (makeAttribute)
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
      layout
        ( do
          title_ $ toHtml $ _talkName talk
          meta_
            [ makeAttribute "property" "og:description"
            , name_ "description"
            , content_ $ _talkDescription talk
            ]
          meta_
            [ makeAttribute "property" "og:url"
            , content_ $ "https://ted2srt.org/talks/" <> _talkSlug talk
            ]
          meta_
            [ makeAttribute "property" "og:title"
            , content_ $ _talkName talk
            ]
          meta_
            [ makeAttribute "property" "og:type"
            , content_ "article"
            ]
          meta_
            [ makeAttribute "property" "og:image"
            , content_ $ _talkImage talk
            ]
          script_ $ LT.toStrict $
            "window.TALK = " <> encodeToLazyText talk
        )
        bundle
