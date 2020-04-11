module Component.Header
  ( Message(..)
  , Query
  , Action
  , component
  ) where

import Core.Prelude

import Data.Const (Const)
import Data.String as String
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as Event
import Web.HTML as Web
import Web.HTML.Location as Location
import Web.HTML.Window as Window

type Message = Void

type Query = Const Void

data Action
  = OnSubmit Event.Event
  | OnValueInput String

type HTML = H.ComponentHTML Action () Aff

type DSL = H.HalogenM State Action () Message Aff

type State =
  { value :: String
  }

initialState :: String -> State
initialState value =
  { value
  }

renderForm :: State -> HTML
renderForm state =
  HH.form
  [ class_ "flex-1 ml-4 lg:ml-8"
  , HP.action "/search"
  , HE.onSubmit $ Just <<< OnSubmit
  ]
  [ HH.input
    [ class_ "border w-full py-2 px-2 focus:border-red500 outline-none"
    , HP.type_ HP.InputSearch
    , HP.value state.value
    , HP.name "q"
    , HP.placeholder "TED talk url or keywords"
    , HE.onValueInput $ Just <<< OnValueInput
    ]
  ]

render :: State -> HTML
render state =
  HH.div
  [ class_ "px-4 xl:px-0 bg-white border-b border-grey300 py-4"]
  [ HH.div
    [ class_ "container flex items-center"]
    [ HH.a
      [ class_ "font-mono text-xl text-red500 no-underline tracking-tight"
      , HP.href "/"
      ]
      [ HH.text "∷ TED → [SRT]" ]
    , renderForm state
    ]
  ]

component :: String -> H.Component HH.HTML Query Unit Message Aff
component q = H.mkComponent
  { initialState: const $ initialState q
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }

tedPrefix :: String
tedPrefix = "https://www.ted.com/talks/"

getTalkPageUrl :: String -> String
getTalkPageUrl slug = "/talks/" <> slug

handleAction :: Action -> DSL Unit
handleAction = case _ of
  OnSubmit event -> do
    state <- H.get
    case String.stripPrefix (String.Pattern tedPrefix) state.value of
      Nothing -> pure unit
      Just slug -> do
        H.liftEffect $ do
          Event.preventDefault event
          Web.window >>= Window.location >>=
            Location.assign (getTalkPageUrl slug)

  OnValueInput value -> do
    H.modify_ $ _ { value = value }
