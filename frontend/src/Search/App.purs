module Search.App where

import Core.Prelude

import Component.Header as Header
import Component.Footer as Footer
import Core.Api as Api
import Core.Model (Talk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type PageData =
  { q :: String
  }

data Query a = Init a

type State =
  { q :: String
  , talks :: Array Talk
  , loading :: Boolean
  }

type HTML = H.ComponentHTML Query

initialState :: PageData -> State
initialState pageData =
  { q: pageData.q
  , talks: []
  , loading: false
  }

renderTalk :: Talk -> HTML
renderTalk talk =
  HH.li
  [ class_ "flex flex-col lg:flex-row mb-6"]
  [ HH.a
    [ class_ "mr-4 flex-no-shrink Image"
    , HP.href $ "/talks/" <> talk.slug
    ]
    [ HH.img
      [ class_ "w-full h-full"
      , HP.src talk.image
      ]
    ]
  , HH.div_
    [ HH.h3
      [ class_ "mb-1 lg:mb-3" ]
      [ HH.a
        [ class_ "Link"
        , HP.href $ "/talks/" <> talk.slug
        ]
        [ HH.text talk.name ]
      ]
    , HH.p
      [ class_ "leading-normal text-grey500"]
      [ HH.text talk.description]
    ]
  ]

render :: State -> HTML
render state =
  HH.div_
  [ Header.render
  , HH.div
    [ class_ "container py-6 px-4 xl:px-0"] $ join
    [ pure $ HH.ul_ $
      state.talks <#> renderTalk
    , guard state.loading $>
      HH.div
      [ class_ "text-center"]
      [ HH.text "loading..."]
    ]
  , Footer.render
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
  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (Init n) = n <$ do
    H.modify_ $ _ { loading = true }
    H.fork $ H.liftAff (Api.searchTalks pageData.q) >>= traverse_ \talks ->
      H.modify_ $ _
        { talks = talks
        , loading = false
        }
