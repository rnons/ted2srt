module Config where

import           Data.Maybe                  (fromMaybe)
import qualified Database.PostgreSQL.Simple  as DB
import qualified Database.Redis              as KV
import           Network.HTTP.Client.Conduit (HasHttpManager (..), Manager,
                                              newManager)
import           Network.Socket.Internal     (PortNumber)
import           RIO
import           Static
import           System.Environment          (getEnv, lookupEnv)

data Config = Config
  { devMode      :: Bool
  , dbConn       :: DB.Connection
  , kvConn       :: KV.Connection
  , httpManager  :: Manager
  , logFunc      :: LogFunc
  , lookupStatic :: LookupStatic
  }

instance HasHttpManager Config where
  getHttpManager = httpManager

instance HasLogFunc Config where
  logFuncL = lens logFunc (\x y -> x { logFunc = y })

getConfig :: IO Config
getConfig = do
  devMode <- (== "true") <$> getEnv "DEVELOPMENT"
  dbName <- getEnv "DB_NAME"
  dbUser <- getEnv "DB_USER"
  dbPassword <- (fromMaybe "" ) <$> lookupEnv "DB_PASSWORD"
  (kvPort :: PortNumber) <- (fromMaybe 6369) <$> readMaybe <$> getEnv "REDIS_PORT"
  db <- DB.connect DB.defaultConnectInfo
      { DB.connectDatabase = dbName
      , DB.connectUser = dbUser
      , DB.connectPassword = dbPassword
      }
  kv <- KV.connect KV.defaultConnectInfo
      { KV.connectPort = KV.PortNumber $ fromIntegral kvPort }
  httpManager <- newManager
  logOptions <- logOptionsHandle stderr True

  lookupStatic <- static
  withLogFunc logOptions $ \lf ->
    pure $ Config devMode db kv httpManager lf lookupStatic
