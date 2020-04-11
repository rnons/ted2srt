module Config where

import qualified Data.ByteString.Char8       as C
import           Data.Maybe                  (fromMaybe)
import           Database.Persist.Postgresql (createPostgresqlPool)
import           Database.Persist.Sql        (ConnectionPool)

import           Control.Monad.Logger        (runLoggingT)
import qualified Database.Redis              as KV
import           Network.HTTP.Client.Conduit (HasHttpManager (..), Manager,
                                              newManager)
import           Network.Socket              (PortNumber)
import           RIO
import           Static
import           System.Environment          (getEnv)

data Config = Config
  { devMode      :: Bool
  , dbPool       :: ConnectionPool
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
  dbConnString <- getEnv "DB_CONN_STRING"
  (kvPort :: PortNumber) <- (fromMaybe 6369) <$> readMaybe <$> getEnv "REDIS_PORT"
  let
    logFunc = \_ _ _ _ -> pure ()
  pool <- flip runLoggingT logFunc $
    createPostgresqlPool (C.pack dbConnString) 10
  kv <- KV.checkedConnect KV.defaultConnectInfo
      { KV.connectPort = KV.PortNumber $ fromIntegral kvPort }
  httpManager <- newManager
  logOptions <- logOptionsHandle stderr True

  lookupStatic <- static
  withLogFunc logOptions $ \lf ->
    pure $ Config devMode pool kv httpManager lf lookupStatic
