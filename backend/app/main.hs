{-# LANGUAGE TypeOperators #-}

import           LoadEnv                              (loadEnv)
import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Servant
import           Servant.Server                       (hoistServer)
import           System.Environment                   (getEnv)

import           Config                               (Config (..), getConfig)
import           Database.Beam.Migrate                (defaultMigratableDbSettings)
import           Database.Beam.Migrate.Simple         (autoMigrate)
import           Database.Beam.Postgres               (runBeamPostgresDebug)
import           Database.Beam.Postgres.Migrate       (migrationBackend)
import           Model                                (talkDbMigration)
import           RIO
import           Server                               (allApi, getBundleH,
                                                       tedApiView, tedServer)


app :: Config -> Application
app config =
  logStdout $ serve allApi $
    (hoistServer tedApiView (runRIO config) (tedServer config)) :<|> getBundleH

main :: IO ()
main = do
  loadEnv
  port <- read <$> getEnv "PORT"
  config <- getConfig
  -- runBeamPostgresDebug putStrLn (dbConn config) $
  --   autoMigrate migrationBackend talkDbMigration
  run port $ app config
