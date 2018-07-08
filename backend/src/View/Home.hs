module View.Home where

import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy  as LT
import           Lucid
import           Lucid.Base      (makeAttribute)
import           Models.Talk     (getTalks)
import           RIO
import           Types
import           View.Bundle
import           View.Layout     (layout)

getHomeH :: AppM (Html ())
getHomeH = do
  talks <- lift $ getTalks 0 20
  bundle <- includeBundle HomeBundle
  layout
    ( do
      title_ "TED2srt: Download bilingual subtitles of TED talks"
      meta_
        [ makeAttribute "property" "og:description"
        , name_ "description"
        , content_ "Download bilingual subtitles and transcripts of TED talks. TED演讲双语字幕下载。"
        ]
      meta_
        [ makeAttribute "property" "og:url"
        , content_ "https://ted2srt.org"
        ]
      meta_
        [ makeAttribute "property" "og:title"
        , content_ "TED2srt: Download bilingual subtitles of TED talks"
        ]
      meta_
        [ makeAttribute "property" "og:type"
        , content_ "website"
        ]
      meta_
        [ makeAttribute "property" "og:image"
        , content_ "https://ted2srt.org/dist/icon.png"
        ]
      script_ $ LT.toStrict $
        "window.TALKS = " <> encodeToLazyText talks
    )
    bundle
