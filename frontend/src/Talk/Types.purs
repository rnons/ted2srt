module Talk.Types where

import Core.Prelude

import Component.Header as Header
import Core.Model (Talk)
import Data.Const (Const)
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

type Slot = ( header :: H.Slot Header.Query Header.Message Unit )

_header = SProxy :: SProxy "header"

type HTML = H.ComponentHTML Action Slot Aff

type DSL = H.HalogenM State Action Slot Void Aff

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
