module Config where

import qualified Control.Monad.Logger        as Logger
import qualified Data.ByteString.Char8       as C
import           Data.Maybe                  (fromMaybe)
import qualified Database.Persist.Postgresql as PG
import           Database.Persist.Sql        (ConnectionPool)
import qualified Database.Redis              as KV
import           Model
import           Network.HTTP.Client.Conduit (HasHttpManager (..), Manager,
                                              newManager)
import           Network.Socket              (PortNumber)
import           RIO
import           Static
import           System.Environment          (getEnv)
import           System.IO                   (print)

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

mkConfig :: IO Config
mkConfig = do
  devMode <- (== "true") <$> getEnv "DEVELOPMENT"
  dbConnString <- getEnv "DB_CONN_STRING"
  (kvPort :: PortNumber) <- (fromMaybe 6369) <$> readMaybe <$> getEnv "REDIS_PORT"
  kv <- KV.checkedConnect KV.defaultConnectInfo
      { KV.connectPort = KV.PortNumber $ fromIntegral kvPort }
  httpManager <- newManager
  logOptions <- logOptionsHandle stderr True

  let
    emptyLogFunc _ _ _ _ = pure ()
    logFunc loc source level str =
      print $ Logger.defaultLogStr loc source level str
  pool <- flip Logger.runLoggingT (if devMode then logFunc else emptyLogFunc) $
    PG.createPostgresqlPool (C.pack dbConnString) 10
  Logger.runLoggingT (PG.runSqlPool (PG.runMigration migrateAll) pool) logFunc

  lookupStatic <- static
  withLogFunc logOptions $ \lf ->
    pure $ Config devMode pool kv httpManager lf lookupStatic
