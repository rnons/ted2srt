module View.Talk where

import           Data.Aeson.Text  (encodeToLazyText)
import qualified Data.Text.Lazy   as LT
import           Database.Persist
import           Lucid
import           Lucid.Base       (makeAttribute)
import           Models.Talk      (getTalkBySlug)
import           RIO
import           Types
import           View.Bundle
import           View.Error
import           View.Layout      (layout)

getTalkH :: Text -> AppM (Html ())
getTalkH slug = do
  mTalk <- lift $ getTalkBySlug slug
  case mTalk of
    Nothing   -> get404H
    Just entity@(Entity _ talk) -> do
      bundle <- includeBundle TalkBundle
      layout
        ( do
          title_ $ toHtml $ talkName talk
          meta_
            [ makeAttribute "property" "og:description"
            , name_ "description"
            , content_ $ talkDescription talk
            ]
          meta_
            [ makeAttribute "property" "og:url"
            , content_ $ "https://ted2srt.org/talks/" <> talkSlug talk
            ]
          meta_
            [ makeAttribute "property" "og:title"
            , content_ $ talkName talk
            ]
          meta_
            [ makeAttribute "property" "og:type"
            , content_ "article"
            ]
          meta_
            [ makeAttribute "property" "og:image"
            , content_ $ talkImage talk
            ]
          script_ $ LT.toStrict $
            "window.TALK = " <> encodeToLazyText (entityIdToJSON entity)
        )
        bundle
