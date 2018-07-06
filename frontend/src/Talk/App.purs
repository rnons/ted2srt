module Talk.App
  ( app
  ) where

import Core.Prelude
import Talk.Types

import Component.Header as Header
import Core.Api as Api
import Core.Model (Talk)
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Talk.Sidebar as Sidebar

type PageData =
  { talk :: Talk
  }

initialState :: PageData -> State
initialState pageData =
  { talk: pageData.talk
  , transcript: []
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

renderTranscript :: State -> HTML
renderTranscript state =
  HH.article
  [ class_ "mt-4 leading-normal"] $
  state.transcript <#> \paragraph ->
    HH.p
    [ class_ "mb-3"]
    [ HH.text paragraph]


render :: State -> HTML
render state@{ talk } =
  HH.div_
  [ Header.render
  , HH.div
    [ class_ "TalkApp container py-6"]
    [ HH.div_
      [ renderTalkInfo talk
      , renderTranscript state
      ]
    , Sidebar.render talk
    ]
  ]

app :: forall m. MonadAff m => PageData -> H.Component HH.HTML Query Unit Void m
app pageData = H.lifecycleComponent
  { initialState: const $ initialState pageData
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  }
  where
  eval :: Query ~> DSL m
  eval (Init n) = n <$ do
    H.liftAff (Api.getTalkTranscript pageData.talk) >>= traverse_ \txt ->
      H.modify_ $ _ { transcript = String.split (String.Pattern "\n") txt }
