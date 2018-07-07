module Talk.Sidebar
  ( render
  ) where

import Core.Prelude
import Talk.Types

import Core.Model (Talk)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Video =
  { name :: String
  , rate :: String
  , resolution :: String
  }

mkVideo :: String -> String -> String -> Video
mkVideo = { name: _, rate: _, resolution: _}

videos :: Array Video
videos =
  [ mkVideo "720p" "1500k" "1280x720"
  ]

titleCls :: String
titleCls = "text-sm font-normal text-grey500"

renderVideo :: HTML
renderVideo =
  HH.div_
  [ HH.h4 [ class_ titleCls]
    [ HH.text "Download Video"]
  , HH.ul [ class_ "py-1 mb-4"] $
    videos <#> \video ->
      HH.li [ class_ "py-1"]
      [ HH.text video.name ]
  ]

renderLanguages :: Talk -> HTML
renderLanguages talk =
  HH.div_
  [ HH.h4 [ class_ titleCls]
    [ HH.text "Select Languages"]
  , HH.ul [ class_ "py-1 mb-4"] $
    talk.languages <#> \language ->
      HH.li
      [ class_ "py-1"
      , HE.onClick $ HE.input_ $ OnClickLang language.languageCode
      ]
      [ HH.text language.languageName]
  ]

render :: Talk -> HTML
render talk =
  HH.div
  [ style "width: 14rem;"]
  [ renderVideo
  , renderLanguages talk
  ]
