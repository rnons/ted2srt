module Talk.Types where

import Core.Prelude
import Core.Model (Talk)
import Halogen as H
import Foreign.Object as FO

data SelectedLang
  = NoLang
  | OneLang String
  | TwoLang String String

data Query a
  = Init a
  | OnClickLang String a

type State =
  { talk :: Talk
  , selectedLang :: SelectedLang
  , transcripts :: FO.Object (Array String)
  }

type HTML = H.ComponentHTML Query

type DSL m = H.ComponentDSL State Query Void m
