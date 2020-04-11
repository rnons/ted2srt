{-# LANGUAGE FlexibleContexts #-}

module Types
  ( AppRIO
  , AppM
  , runDB
  , module X
  ) where

import           Config                      as X (Config (..))
import           Control.Monad.Except        (ExceptT)
import qualified Database.Persist.Postgresql as PG
import           Database.Persist.Sql        (SqlPersistT)
import           Model                       as X (Talk (..))
import           RIO
import           Servant                     (ServantErr)

type AppRIO = RIO Config

type AppM = ExceptT ServantErr (RIO Config)

runDB :: SqlPersistT AppRIO a -> AppRIO a
runDB action = do
  Config { dbPool } <- ask
  PG.runSqlPool action dbPool
