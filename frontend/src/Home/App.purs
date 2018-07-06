module Home.App
  ( Query
  , app
  ) where

import Core.Prelude

import Core.Api as Api
import Core.Model (Talk, getTitleSpeaker)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type PageData =
  { talks :: Array Talk
  }

data Query a
  = Init a

type State =
  { talks :: Array Talk
  }

type HTML = H.ComponentHTML Query

type DSL m = H.ComponentDSL State Query Void m

initialState :: PageData -> State
initialState pageData =
  { talks: pageData.talks
  }

renderTalk :: Talk -> HTML
renderTalk talk =
  HH.li
  []
  [ HH.a
    [ class_ "Link"
    , HP.href $ "/talks/" <> talk.slug
    ]
    [ HH.img
      [ HP.src talk.image ]
    , HH.div [ class_ "py-2"]
      [ HH.h3_
        [ HH.text title ]
      , HH.div [ class_ "mt-1 text-grey500"]
        [ HH.text speaker ]
      ]
    ]
  ]
  where
  Tuple title speaker = getTitleSpeaker talk

render :: State -> HTML
render state =
  HH.div
  [ class_ "container" ]
  [ HH.text "TED2srt"
  , HH.ul [ class_ "HomeGrid"] $
    state.talks <#> renderTalk
  ]

app :: forall m. MonadAff m => PageData -> H.Component HH.HTML Query Unit Void m
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
