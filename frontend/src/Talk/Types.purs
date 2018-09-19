module Talk.Types where

import Core.Prelude
import Core.Model (Talk)
import Halogen as H
import Foreign.Object as FO

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
  | ToggleAudioPlay a
  | StopAudioPlay a

type State =
  { talk :: Talk
  , selectedLang :: SelectedLang
  , transcripts :: FO.Object (Array String)
  , playing :: Boolean
  , audioPlaying :: Boolean
  }

type HTML = H.ComponentHTML Query

type DSL m = H.ComponentDSL State Query Void m

initialState :: PageData -> State
initialState pageData =
  { talk: pageData.talk
  , selectedLang: NoLang
  , transcripts: FO.empty
  , playing: false
  , audioPlaying: false
  }
