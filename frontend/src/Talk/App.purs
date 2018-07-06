module Talk.App
  ( Query
  , app
  ) where

import Core.Prelude

import Core.Model (Talk)
import Halogen as H
import Halogen.HTML as HH

type PageData =
  { talk :: Talk
  }

data Query a
  = Init a

type State =
  { talk :: Talk
  }

type HTML = H.ComponentHTML Query

type DSL m = H.ComponentDSL State Query Void m

initialState :: PageData -> State
initialState pageData =
  { talk: pageData.talk
  }

render :: State -> HTML
render { talk } =
  HH.div_
  [ HH.text talk.name
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
