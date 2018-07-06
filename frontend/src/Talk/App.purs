module Talk.App
  ( app
  ) where

import Core.Prelude

import Component.Header as Header
import Core.Model (Talk)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Talk.Sidebar as Sidebar
import Talk.Types

type PageData =
  { talk :: Talk
  }

initialState :: PageData -> State
initialState pageData =
  { talk: pageData.talk
  }

renderTalkInfo :: Talk -> HTML
renderTalkInfo talk =
  HH.div_
  [ HH.h1 [ class_ "text-lg mb-3"]
    [ HH.text talk.name ]
  , HH.div [ class_ "flex"]
    [ HH.img
      [ style "width: 16rem; height: 9rem;"
      , HP.src talk.image
      ]
    , HH.p [ class_ "mx-3 leading-normal"]
      [ HH.text talk.description ]
    ]
  ]

render :: State -> HTML
render { talk } =
  HH.div_
  [ Header.render
  , HH.div
    [ class_ "TalkApp container py-6"]
    [ HH.div_
      [ renderTalkInfo talk ]
    , Sidebar.render talk
    ]
  ]

app :: forall m. PageData -> H.Component HH.HTML Query Unit Void m
app pageData = H.component
  { initialState: const $ initialState pageData
  , render
  , eval
  , receiver: const Nothing
  }
  where
  eval :: Query ~> DSL m
  eval (Init n) = n <$ do
    pure unit
