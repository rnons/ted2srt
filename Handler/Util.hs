{-# LANGUAGE OverloadedStrings  #-}
module Handler.Util where

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (id)

import Model
import Web.TED

tedTalkUrl :: Text -> Text
tedTalkUrl s = "http://www.ted.com/talks/" <> s <> ".html"

marshal :: Web.TED.Talk -> IO Model.Talk
marshal talk = do
    (mediaSlug, mediaPad) <- getSlugAndPad $ tedTalkUrl $ slug talk
    return Model.Talk { talkTid = id talk
                      , talkName = name talk
                      , talkDescription = description talk
                      , talkSlug = slug talk
                      , talkLink = tedTalkUrl $ slug talk
                      , talkPublishedAt = published_at talk
                      , talkImage = talkImg talk
                      , talkMediaSlug = mediaSlug
                      , talkMediaPad = mediaPad
                      }

talkUrl, newTalkUrl :: Text
talkUrl = "http://www.ted.com/talks/"
newTalkUrl = "http://new.ted.com/talks/"

downloadUrl :: Text
downloadUrl = "http://download.ted.com/talks/"

-- Available quality: 1500k, 950k, 600k, 450k, 320k, 180k, 64k
mediaUrl :: Text -> Text -> Text
mediaUrl part quality =
  downloadUrl <> part <> "-" <> quality <> ".mp4"

img113x85 :: Text -> Text
img113x85 rurl = (T.reverse. T.drop 11 . T.reverse) rurl <> "113x85.jpg"

-- Drop the talkUrl part.
-- e.g. marla_spivak_why_bees_are_disappearing.html
rewriteUrl ::Text -> Text
rewriteUrl = T.drop $ T.length talkUrl
