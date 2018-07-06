module Talk.Types where

import Core.Prelude
import Core.Model (Talk)
import Halogen as H

data Query a
  = Init a

type State =
  { talk :: Talk
  , transcript :: Array String
  }

type HTML = H.ComponentHTML Query

type DSL m = H.ComponentDSL State Query Void m
