module Config where

import           Data.Maybe                  (fromMaybe)
import qualified Database.PostgreSQL.Simple  as DB
import qualified Database.Redis              as KV
import           Network.HTTP.Client.Conduit (Manager, newManager)
import           Network.Socket.Internal     (PortNumber)
import           RIO
import           System.Environment          (getEnv, lookupEnv)

data Config = Config
    { dbConn      :: DB.Connection
    , kvConn      :: KV.Connection
    , httpManager :: Manager
    }

getConfig :: IO Config
getConfig = do
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
    return $ Config db kv httpManager
