module View.Home where

import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy  as LT
import           Lucid
import           Models.Talk     (getTalks)
import           RIO
import           Types
import           View.Bundle
import           View.Layout     (layout)

getHomeH :: AppM (Html ())
getHomeH = do
  talks <- lift $ getTalks 0 20
  bundle <- includeBundle HomeBundle
  pure $ layout
    ( do
      title_ "TED2srt: Download bilingual subtitles of TED talks"
      script_ $ LT.toStrict $
        "window.TALKS = " <> encodeToLazyText talks
    )
    bundle
