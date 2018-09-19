module Talk.App
  ( app
  ) where

import Core.Prelude

import Component.Footer as Footer
import Component.Header as Header
import Core.Api as Api
import Core.Model (unescape)
import Data.Array as Array
import Data.Foldable (sequence_)
import Data.MediaType (MediaType(..))
import Data.Monoid as Monoid
import Data.String as String
import Foreign.Object as FO
import Halogen (Namespace(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Simple.JSON (readJSON, writeJSON)
import Talk.Sidebar as Sidebar
import Talk.Types (DSL, HTML, PageData, Query(..), SelectedLang(..), State, initialState)
import Talk.Util as Util
import Web.Event.Event (EventType(..))
import Web.HTML (HTMLMediaElement, window)
import Web.HTML.HTMLElement as HTML
import Web.HTML.HTMLMediaElement as Media
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

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

audioRef :: H.RefLabel
audioRef = H.RefLabel "audio"

renderAudio :: State -> HTML
renderAudio state@{ talk } =
  HH.div
  [ class_ "fixed flex items-center"
  , style "bottom: 2rem; right: 2rem;"
  ]
  [ HH.div
    [ class_ $ "flex" <> Monoid.guard (not state.audioPlaying) " hidden"
    ]
    [ HH.button
      [ class_ $ btnCls <> " w-8 h-8 mr-3"
      , HE.onClick $ HE.input_ OnAudioBackward
      ]
      [ HH.text "⏪"]
    , HH.button
      [ class_ $ btnCls <> " w-8 h-8 mr-3"
      , HE.onClick $ HE.input_ OnAudioForward
      ]
      [ HH.text "⏩"]
    , HH.button
      [ class_ $ btnCls <> " w-8 h-8 mr-3"
      , HE.onClick $ HE.input_ OnStopAudioPlay
      ]
      [ HH.text "⏹"]
    ]
  , HH.div
    [ class_ "relative select-none"
    , HE.onClick $ HE.input_ OnToggleAudioPlay
    ]
    [ HH.div
      [ class_ $ btnCls <> " w-12 h-12 border-none"]
      [ HH.text "🎵" ]
    , HH.elementNS svgNS (H.ElemName "svg")
      [ svgClass_ "absolute pin"
      , HH.attr (HH.AttrName "viewBox") "0 0 48 48"]
      [ HH.elementNS svgNS (H.ElemName "circle")
        [ svgAttr "cx" "24"
        , svgAttr "cy" "24"
        , svgAttr "r" "23"
        , svgAttr "fill" "none"
        , svgAttr "stroke" "rgba(0,0,0,0.12)"
        , svgAttr "stroke-width" "2"
        ]
        []
      ]
    , HH.elementNS svgNS (H.ElemName "svg")
      [ svgClass_ "absolute pin"
      , style "transform: rotate(-90deg)"
      , HH.attr (HH.AttrName "viewBox") "0 0 48 48"]
      [ HH.elementNS svgNS (H.ElemName "circle")
        [ svgAttr "cx" "24"
        , svgAttr "cy" "24"
        , svgAttr "r" "23"
        , svgAttr "fill" "none"
        , svgAttr "stroke" "#70c542"
        , svgAttr "stroke-width" "2"
        , svgAttr "stroke-dasharray" $
            show (state.audioProgress * perimeter) <> " " <> show perimeter
        ]
        []
      ]
    ]
  , HH.audio
    [ class_ ""
    , HP.src url
    , HP.ref audioRef
    ] []
  ]
  where
  btnCls = "flex items-center justify-center border rounded-full bg-white cursor-pointer"
  url = "https://download.ted.com/talks/" <> talk.mediaSlug <> ".mp3"
  svgNS :: Namespace
  svgNS = Namespace "http://www.w3.org/2000/svg"
  svgAttr name value = HH.attr (HH.AttrName name) value
  perimeter = 3.14 * 48.0

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
    , renderAudio state
    ]
  , Footer.render
  ]

app :: PageData -> H.Component HH.HTML Query Unit Void Aff
app pageData@{ talk } = H.component
  { initialState: const $ initialState pageData
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  }
  where
  fetchTranscript :: String -> DSL Unit
  fetchTranscript lang = do
    state <- H.get
    when (not $ FO.member lang state.transcripts) $
      H.liftAff (Api.getTalkTranscript talk lang) >>= traverse_ \txt ->
        let
          transcript = String.split (String.Pattern "\n") txt
        in
          H.modify_ $ \s -> s
            { transcripts = FO.insert lang transcript s.transcripts
            }

  isLangAvailable :: String -> Boolean
  isLangAvailable langCode =
    isJust $ Array.findIndex (\lang -> lang.languageCode == langCode) talk.languages

  withAudioPlayer :: (HTMLMediaElement -> DSL Unit) -> DSL Unit
  withAudioPlayer actions =
    H.getHTMLElementRef audioRef >>= traverse_ \el -> do
      for_ (Media.fromHTMLElement el) actions

  eval :: Query ~> DSL
  eval (Init n) = n <$ do
    selectedLang <- H.liftEffect $ window >>= Window.localStorage >>=
      Storage.getItem "languages" >>= \ml -> pure $ case ml of
        Nothing -> OneLang "en"
        Just languages -> case readJSON languages of
          Right [lang] ->
            if isLangAvailable lang
            then OneLang lang
            else OneLang "en"
          Right [lang1, lang2] ->
            case isLangAvailable lang1, isLangAvailable lang2 of
              true, true -> TwoLang lang1 lang2
              true, false -> OneLang lang1
              false, true -> OneLang lang2
              false, false -> OneLang "en"
          _ -> OneLang "en"
    H.modify_ $ _ { selectedLang = selectedLang }

    case selectedLang of
      NoLang -> pure unit
      OneLang lang -> fetchTranscript lang
      TwoLang lang1 lang2 ->
        -- Cannot use H.fork here, seems postgresql-simple cannot handle
        -- concurrent requests well
        fetchTranscript lang1 *> fetchTranscript lang2

    H.getHTMLElementRef audioRef >>= traverse_ \el -> do
      H.subscribe $
        ES.eventListenerEventSource (EventType "timeupdate") (HTML.toEventTarget el)
          (Just <<< H.action <<< HandleAudioProgress)

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

  eval (HandleAudioProgress event n) = n <$ do
    withAudioPlayer \audio -> do
      percentage <- H.liftEffect $ do
        currentTime <- Media.currentTime audio
        duration <- Media.duration audio
        pure $ currentTime / duration
      H.modify_ $ _ { audioProgress = percentage }

  eval (OnToggleAudioPlay n) = n <$ do
    state <- H.modify $ \s -> s { audioPlaying = not s.audioPlaying }
    withAudioPlayer \audio -> H.liftEffect $ do
      paused <- Media.paused audio
      (if paused then Media.play else Media.pause) audio

  eval (OnStopAudioPlay n) = n <$ do
    H.modify_ $ \s -> s { audioPlaying = false }
    withAudioPlayer \audio -> H.liftEffect $ do
      Media.setCurrentTime 0.0 audio
      Media.pause audio

  eval (OnAudioBackward n) = n <$ do
    withAudioPlayer \audio -> H.liftEffect $ do
      currentTime <- Media.currentTime audio
      duration <- Media.duration audio
      Media.setCurrentTime (max (currentTime - 10.0) 0.0) audio

  eval (OnAudioForward n) = n <$ do
    withAudioPlayer \audio -> H.liftEffect $ do
      currentTime <- Media.currentTime audio
      duration <- Media.duration audio
      Media.setCurrentTime (min (currentTime + 10.0) duration) audio
