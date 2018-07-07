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
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Simple.JSON (read, readJSON, writeJSON)
import Talk.Sidebar as Sidebar
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

type PageData =
  { talk :: Talk
  }

initialState :: PageData -> State
initialState pageData =
  { talk: pageData.talk
  , selectedLang: NoLang
  , transcripts: FO.empty
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
    , HH.p [ class_ "mx-3 leading-normal text-grey500"]
      [ HH.text talk.description ]
    ]
  ]

renderTranscriptLang :: State -> String -> HTML
renderTranscriptLang state lang =
  case FO.lookup lang state.transcripts of
    Nothing -> HH.text ""
    Just transcript -> HH.div_ $
      transcript <#> \paragraph ->
        HH.p
        [ class_ "mb-3"]
        [ HH.text paragraph]

renderTranscript :: State -> HTML
renderTranscript state = trace state.transcripts $ \_ ->
  HH.article
  [ class_ "mt-4 leading-normal"]
  [ case state.selectedLang of
      NoLang -> HH.text "Select language from sidebar."
      OneLang lang ->
        renderTranscriptLang state lang
      TwoLang lang1 lang2 ->
        HH.div
        [ style "display: grid; grid-template-columns: 1fr 1fr; grid-gap: 2rem;"]
        [ renderTranscriptLang state lang1
        , renderTranscriptLang state lang2
        ]
  ]

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
  fetchTranscript :: String -> DSL m Unit
  fetchTranscript lang = do
    state <- H.get
    when (not $ FO.member lang state.transcripts) $
      H.liftAff (Api.getTalkTranscript pageData.talk lang) >>= traverse_ \txt ->
        let
          transcript = String.split (String.Pattern "\n") txt
        in
          H.modify_ $ \s -> s
            { transcripts = FO.insert lang transcript s.transcripts
            }

  eval :: Query ~> DSL m
  eval (Init n) = n <$ do
    selectedLang <- H.liftEffect $ window >>= Window.localStorage >>=
      Storage.getItem "languages" >>= \ml -> pure $ case ml of
        Nothing -> OneLang "en"
        Just languages -> case readJSON languages of
          Right [lang] -> OneLang lang
          Right [lang1, lang2] -> TwoLang lang1 lang2
          _ -> OneLang "en"
    H.modify_ $ _ { selectedLang = selectedLang }

    case selectedLang of
      NoLang -> pure unit
      OneLang lang -> fetchTranscript lang
      TwoLang lang1 lang2 ->
        H.fork (fetchTranscript lang1) *> fetchTranscript lang2

  eval (OnClickLang language n) = n <$ do
    fetchTranscript language
    state <- H.get
    let
      selectedLang = case state.selectedLang of
        NoLang -> OneLang language
        OneLang lang ->
          if lang == language
          then NoLang
          else TwoLang lang language
        TwoLang lang1 lang2 ->
          if lang1 == language
          then OneLang lang2
          else
            if lang2 == language
            then OneLang lang1
            else TwoLang lang1 lang2
    H.modify_ $ _ { selectedLang = selectedLang }
    let
      mLangs = case selectedLang of
        NoLang -> Nothing
        OneLang lang -> Just [lang]
        TwoLang lang1 lang2 -> Just [lang1, lang2]
    for_ mLangs $ \langs ->
      H.liftEffect $ window >>= Window.localStorage >>=
        Storage.setItem "languages" (writeJSON langs)
