module Talk.App
  ( app
  ) where

import Core.Prelude
import Talk.Types

import Component.Footer as Footer
import Component.Header as Header
import Core.Api as Api
import Core.Model (Talk, unescape)
import Data.Array as Array
import Data.Foldable (sequence_)
import Data.MediaType (MediaType(..))
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON (read, readJSON, writeJSON)
import Talk.Sidebar as Sidebar
import Talk.Util as Util
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
  , playing: false
  }

renderTalkInfo :: State -> HTML
renderTalkInfo { talk, selectedLang, playing } =
  HH.div_ $ join
  [ pure $ HH.h1 [ class_ "text-lg mb-3"]
    [ HH.text talk.name ]
  , guard (not playing) $> HH.div [ class_ "flex flex-col lg:flex-row"]
    [ HH.div
      [ class_ "flex-no-shrink cursor-pointer bg-cover bg-center Image"
      , style $ "background-image: url(" <> unescape talk.image <> ")"
      , HE.onClick $ HE.input_ OnClickPlay
      ]
      [ HH.div
        [ class_ "h-full text-white hover:bg-grey300 flex items-center justify-center"]
        [ HH.button
          [ class_ "w-8 h-8 text-white text-3xl PlayButton"
          ]
          [ HH.text "▶" ]
        ]
      ]
    , HH.p [ class_ "mt-2 lg:mt-0 lg:ml-3 leading-normal text-grey500"]
      [ HH.text $ unescape talk.description ]
    ]
  , guard playing $>
    HH.video
    [ class_ "w-full"
    , HP.controls true
    , HP.autoplay true
    ]
    [ HH.source
      [ HP.type_ $ MediaType "video/mp4"
      , HP.src $ Util.mkVideoUrl talk "950k" ]
    , HH.track
      [ HP.src $ Util.mkTranscriptUrl talk selectedLang "vtt"
      , HP.attr (HH.AttrName "kind") "captions"
      , HP.attr (HH.AttrName "default") ""
      ]
    ]
  ]

renderOneTranscript :: State -> String -> HTML
renderOneTranscript state lang =
  case FO.lookup lang state.transcripts of
    Nothing -> HH.text ""
    Just transcript -> HH.div_ $
      transcript <#> \paragraph ->
        HH.p_
        [ HH.text paragraph]

renderTwoTranscripts :: State -> String -> String -> HTML
renderTwoTranscripts state@{ transcripts } lang1 lang2 =
  case FO.lookup lang1 transcripts, FO.lookup lang2 transcripts of
    Nothing, Nothing -> HH.text ""
    Just _, Nothing -> renderOneTranscript state lang1
    Nothing, Just _ -> renderOneTranscript state lang2
    Just transcript1, Just transcript2 -> HH.div_ $
      Array.zip transcript1 transcript2 <#> \(Tuple p1 p2) ->
        HH.div
        [ class_ "Row pb-4 lg:pb-0" ]
        [ HH.p_
          [ HH.text p1]
        , HH.p_
          [ HH.text p2]
        ]

renderTranscript :: State -> HTML
renderTranscript state =
  HH.article
  [ class_ "mt-6 leading-normal"]
  [ case state.selectedLang of
      NoLang ->
        HH.div
        [ class_ "text-center text-lg mt-8 italic"]
        [ HH.text "Select language from sidebar." ]
      OneLang lang ->
        renderOneTranscript state lang
      TwoLang lang1 lang2 ->
        renderTwoTranscripts state lang1 lang2
  ]

render :: State -> HTML
render state =
  HH.div_
  [ Header.render
  , HH.div
    [ class_ "TalkApp container py-6 px-4 xl:px-0"]
    [ HH.div_
      [ renderTalkInfo state
      , renderTranscript state
      ]
    , Sidebar.render state
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
        -- Cannot use H.fork here, seems postgresql-simple cannot handle
        -- concurrent requests well
        fetchTranscript lang1 *> fetchTranscript lang2

  eval (OnClickLang language n) = n <$ do
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
    case mLangs of
      Nothing -> H.liftEffect $ window >>= Window.localStorage >>=
        Storage.removeItem "languages"
      Just langs -> do
        sequence_ $ fetchTranscript <$> langs
        H.liftEffect $ window >>= Window.localStorage >>=
          Storage.setItem "languages" (writeJSON langs)

  eval (OnClickPlay n) = n <$ do
    H.modify_ $ _ { playing = true }
