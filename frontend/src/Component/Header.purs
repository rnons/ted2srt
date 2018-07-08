module Component.Header
  ( render
  ) where

import Core.Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

renderForm :: forall p i. HH.HTML p i
renderForm =
  HH.form
  [ class_ "flex-1 ml-4 lg:ml-8"
  , HP.action "/search"
  ]
  [ HH.input
    [ class_ "border w-full py-2 px-2 focus:border-red500 outline-none"
    , HP.type_ HP.InputSearch
    , HP.name "q"
    , HP.placeholder "TED talk url or keywords"
    ]
  ]

render :: forall p i. HH.HTML p i
render =
  HH.div
  [ class_ "px-4 xl:px-0 bg-white border-b border-grey300 py-4"]
  [ HH.div
    [ class_ "container flex items-center"]
    [ HH.a
      [ class_ "font-mono text-xl text-red500 no-underline tracking-tight"
      , HP.href "/"
      ]
      [ HH.text "∷ TED → [SRT]" ]
    , renderForm
    ]
  ]
