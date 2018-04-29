module Types
  ( AppM
  , module X
  ) where

import           Config as X (Config (..))
import           Model  as X (Talk, TalkT (..))
import           RIO

type AppM = RIO Config
