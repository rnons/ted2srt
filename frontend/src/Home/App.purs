module Home.App
  ( Query
  , app
  ) where

import Core.Prelude

import Component.Footer as Footer
import Component.Header as Header
import Core.Api as Api
import Core.Model (Talk, getTitleSpeaker, unescape)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type PageData =
  { talks :: Array Talk
  }

data Query a
  = LoadMore a

type State =
  { talks :: Array Talk
  , loading :: Boolean
  , hasMore :: Boolean
  }

type HTML = H.ComponentHTML Query () Aff

type DSL = H.HalogenM State Query () Void Aff

initialState :: PageData -> State
initialState pageData =
  { talks: pageData.talks
  , loading: false
  , hasMore: true
  }

renderTalk :: Talk -> HTML
renderTalk talk =
  HH.li_
  [ HH.a
    [ class_ "Link"
    , HP.href $ "/talks/" <> talk.slug
    ]
    [ HH.img
      [ HP.src $ unescape talk.image ]
    , HH.div [ class_ "py-2"]
      [ HH.h3_
        [ HH.text title ]
      , HH.div [ class_ "mt-1 text-grey500 text-sm"]
        [ HH.text speaker ]
      ]
    ]
  ]
  where
  Tuple title speaker = getTitleSpeaker talk

render :: State -> HTML
render state =
  HH.div_
  [ Header.render ""
  , HH.div
    [ class_ "container py-6" ] $ join
    [ pure $ HH.ul [ class_ "HomeApp"] $
      state.talks <#> renderTalk
    , guard state.hasMore $> HH.div [ class_ "mt-8 text-center"] (join
      [ guard (not state.loading) $> HH.button
        [ class_ "border py-2 px-6 outline-none text-grey500 hover:text-red500 hover:border-red500"
        , HE.onClick $ HE.input_ LoadMore
        ]
        [ HH.text "LOAD MORE"]
      , guard state.loading $>
        HH.text "loading..."
      ])
    ]
  , Footer.render
  ]

app :: PageData -> H.Component HH.HTML Query Unit Void Aff
app pageData = H.component
  { initialState: const $ initialState pageData
  , render
  , eval
  , receiver: const Nothing
  , initializer: Nothing
  , finalizer: Nothing
  }
  where
  eval :: Query ~> DSL
  eval (LoadMore n) = n <$ do
    state <- H.modify $ _ { loading = true }
    H.liftAff (Api.getTalks $ Array.length state.talks) >>= traverse_ \talks ->
      H.modify $ \s -> s
        { talks = Array.union s.talks talks
        , loading = false
        , hasMore = Array.length talks == 20
        }
