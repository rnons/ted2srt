module Search.App where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type PageData =
  { q :: String
  }

data Query a = Empty a

type State = { q :: String }

initialState :: PageData -> State
initialState pageData =
  { q: pageData.q }

render :: forall m. State -> H.ComponentHTML Query
render state =
  HH.div_
    [ HH.text state.q ]

eval :: forall m. Query ~> H.ComponentDSL State Query Void m
eval (Empty next) = pure next

app :: forall m. PageData -> H.Component HH.HTML Query Unit Void m
app pageData = H.component
  { initialState: const $ initialState pageData
  , render
  , eval
  , receiver: const Nothing
  }
