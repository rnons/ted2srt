module Talk.Types where

import Core.Prelude

import Core.Model (Talk)
import Foreign.Object as FO
import Halogen as H
import Web.Event.Event as Web

type PageData =
  { talk :: Talk
  }

data SelectedLang
  = NoLang
  | OneLang String
  | TwoLang String String

data Query a
  = Init a
  | OnClickLang String a
  | OnClickPlay a
  | HandleAudioProgress Web.Event a
  | HandleAudioPlay Web.Event a
  | HandleAudioPause Web.Event a
  | HandleAudioError a
  | OnToggleAudioControls a
  | OnToggleAudioPlay a
  | OnStopAudioPlay a
  | OnAudioBackward a
  | OnAudioForward a

type State =
  { talk :: Talk
  , selectedLang :: SelectedLang
  , transcripts :: FO.Object (Array String)
  , playing :: Boolean
  , hasAudio :: Boolean
  , audioPlayerExpanded :: Boolean
  , audioPlaying :: Boolean
  , audioProgress :: Number
  }

type HTML = H.ComponentHTML Query () Aff

type DSL = H.HalogenM State Query () Void Aff

initialState :: PageData -> State
initialState pageData =
  { talk: pageData.talk
  , selectedLang: NoLang
  , transcripts: FO.empty
  , playing: false
  , hasAudio: true
  , audioPlayerExpanded: false
  , audioPlaying: false
  , audioProgress: 0.0
  }
