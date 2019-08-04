module Talk.Types where

import Core.Prelude

import Data.Const (Const)
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

type Query = Const Void

data Action
  = Init
  | OnClickLang String
  | OnClickPlay
  | HandleAudioProgress Web.Event
  | HandleAudioPlay Web.Event
  | HandleAudioPause Web.Event
  | HandleAudioError
  | OnToggleAudioControls
  | OnToggleAudioPlay
  | OnStopAudioPlay
  | OnAudioBackward
  | OnAudioForward

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

type HTML = H.ComponentHTML Action () Aff

type DSL = H.HalogenM State Action () Void Aff

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
