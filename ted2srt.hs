module Main where

import           Control.Monad.Logger (runLoggingT)
import           Database.Persist (loadConfig, applyEnv, createPoolConfig, runPool)
import           Database.Persist.Postgresql (PostgresConf)
import           Database.Persist.Sql (runMigration)
import           Network.Wai.Logger (clockDateCacher)
import           System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import           Yesod (warp, messageLoggerSource)
import           Yesod.Core.Types (Logger (Logger))
import           Yesod.Default.Config (withYamlEnvironment, DefaultEnv(..))

import Foundation (Ted(..))
import Model (migrateAll)
import Settings (staticSite)

main :: IO ()
main = do
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" Production
              loadConfig >>= applyEnv
    p <- createPoolConfig (dbconf :: PostgresConf)
    loggerSet <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher
    let logger = Logger loggerSet getter
        foundation = Ted s p dbconf
    runLoggingT (runPool dbconf (runMigration migrateAll) p)
                (messageLoggerSource foundation logger)
    warp 3000 foundation
