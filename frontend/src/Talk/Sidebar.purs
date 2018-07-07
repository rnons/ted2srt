module Talk.Sidebar
  ( render
  ) where

import Core.Prelude
import Talk.Types

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

itemCls :: String
itemCls = "py-1 px-3 text-sm cursor-pointer"

renderVideo :: HTML
renderVideo =
  HH.div_
  [ HH.h4 [ class_ titleCls]
    [ HH.text "Download Video"]
  , HH.ul [ class_ "py-1 mb-4"] $
    videos <#> \video ->
      HH.li [ class_ itemCls]
      [ HH.text video.name ]
  ]

isLangSelected :: SelectedLang -> String -> Boolean
isLangSelected selected language =
  case selected of
    NoLang -> false
    OneLang lang -> lang == language
    TwoLang lang1 lang2 -> lang1 == language || lang2 == language

renderLanguages :: State -> HTML
renderLanguages { talk, selectedLang } =
  HH.div_
  [ HH.h4 [ class_ titleCls]
    [ HH.text "Select Languages"]
  , HH.ul [ class_ "py-1 mb-4"] $
    talk.languages <#> \language ->
      HH.li
      [ class_ $ getCls language.languageCode
      , style "margin-bottom: 2px;"
      , HE.onClick $ HE.input_ $ OnClickLang language.languageCode
      ] $ join
      [ pure $ HH.text language.languageName
      , guard (isLangSelected selectedLang language.languageCode) $>
        HH.span [ class_ "text-lg"]
        [ HH.text "×" ]
      ]
  ]
  where
  baseCls = itemCls <> " flex items-center justify-between"
  activeCls = baseCls <> " bg-red500 text-white"
  getCls lang = if isLangSelected selectedLang lang then activeCls else baseCls

render :: State -> HTML
render state =
  HH.div
  [ style "width: 14rem;"]
  [ renderVideo
  , renderLanguages state
  ]
