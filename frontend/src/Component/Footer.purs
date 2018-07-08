module Component.Footer
  ( render
  ) where

import Core.Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Link =
  { text :: String
  , href :: String
  }

mkLink :: String -> String -> Link
mkLink = { text: _, href: _ }

itemCls :: String
itemCls = ""

renderSeparator :: forall p i. HH.HTML p i
renderSeparator =
  HH.li
  [ class_ "border-l h-4 mx-5"]
  []

renderLink :: forall p i. Link -> HH.HTML p i
renderLink link =
  HH.li
  [ class_ itemCls ]
  [ HH.a
    [ class_ "Link"
    , HP.href link.href
    ]
    [ HH.text link.text ]
  ]

render :: forall p i. HH.HTML p i
render =
  HH.div
  [ class_ "mt-6 py-4 border-t border-grey300"]
  [ HH.ul
    [ class_ "container flex items-center justify-center"]
    [ HH.li [ class_ "mr-1"]
      [ HH.text "TED2srt by" ]
    , renderLink $ mkLink "rnons" "https://twitter.com/rnons"
    , renderSeparator
    , renderLink $ mkLink "source code" "https://github.com/rnons/ted2srt"
    , renderSeparator
    , renderLink $ mkLink "feed" "/atom.xml"
    ]
  ]
