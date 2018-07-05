module Home.App
  ( Query
  , app
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

data Query a
  = Init a

type State =
  { value :: String
  }

type HTML = H.ComponentHTML Query

type DSL m = H.ComponentDSL State Query Void m

initialState :: State
initialState =
  { value: ""
  }

render :: State -> HTML
render state =
  HH.div_
  [ HH.text "hello world" ]

app :: forall m. H.Component HH.HTML Query Unit Void m
app = H.component
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }
  where
  eval :: Query ~> DSL m
  eval (Init next) = pure next
