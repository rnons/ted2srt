module Home.App
  ( Query
  , app
  ) where

import Core.Prelude

import Core.Api as Api
import Core.Model (Talk)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data Query a
  = Init a

type State =
  { talks :: Array Talk
  }

type HTML = H.ComponentHTML Query

type DSL m = H.ComponentDSL State Query Void m

initialState :: State
initialState =
  { talks: []
  }

renderTalk :: Talk -> HTML
renderTalk talk =
  HH.li
  []
  [ HH.img
    [ HP.src talk.image ]
  ]

render :: State -> HTML
render state =
  HH.div
  [ class_ "container" ]
  [ HH.text "TED2srt"
  , HH.ul_ $
    state.talks <#> renderTalk
  ]

app :: forall m. MonadAff m => H.Component HH.HTML Query Unit Void m
app = H.lifecycleComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  }
  where
  eval :: Query ~> DSL m
  eval (Init n) = n <$ do
    H.liftAff Api.getTalks >>= traverse_ \talks ->
      H.modify_ $ _ { talks = talks }
