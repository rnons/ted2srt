module Types
  ( AppRIO
  , AppM
  , module X
  ) where

import           Config               as X (Config (..))
import           Control.Monad.Except (ExceptT)
import           Model                as X (Talk, TalkT (..))
import           RIO
import           Servant              (ServantErr)

type AppRIO = RIO Config

type AppM = ExceptT ServantErr (RIO Config)
