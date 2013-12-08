import           Control.Monad.Logger (runLoggingT)
import           Database.Persist (loadConfig, applyEnv, createPoolConfig, runPool)
import           Database.Persist.Postgresql (PostgresConf)
import           Database.Persist.Sql (runMigration)
import           System.Log.FastLogger (mkLogger)
import           System.IO (stdout)
import           Yesod (warp, messageLoggerSource)
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
    logger <- mkLogger True stdout
    let foundation = Ted s p dbconf
    runLoggingT (runPool dbconf (runMigration migrateAll) p)
                (messageLoggerSource foundation logger)
    warp 3000 foundation
