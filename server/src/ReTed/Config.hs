module ReTed.Config where

import qualified Database.PostgreSQL.Simple as DB
import qualified Database.Redis as KV
import           System.Environment (getEnv)


data Config = Config
    { dbConn :: DB.Connection
    , kvConn :: KV.Connection
    }

getConfig :: IO Config
getConfig = do
    dbName <- getEnv "DB_NAME"
    dbUser <- getEnv "DB_USER"
    dbPassword <- getEnv "DB_PASSWORD"
    kvPort <- read <$> getEnv "REDIS_PORT"
    db <- DB.connect DB.defaultConnectInfo
        { DB.connectDatabase = dbName
        , DB.connectUser = dbUser
        , DB.connectPassword = dbPassword
        }
    kv <- KV.connect KV.defaultConnectInfo
        { KV.connectPort = KV.PortNumber $ fromIntegral kvPort }
    return $ Config db kv
