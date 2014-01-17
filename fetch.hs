{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (forM_, void, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (Result(..), fromJSON)
import           Data.Maybe (isNothing)
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Yaml
import           Database.Persist.Postgresql
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit (simpleHttp)
import           Text.HTML.DOM (parseLBS)
import           Text.XML.Cursor

import Model
import Handler.Util (marshal)
import Web.TED (queryTalk)


data Database = Database
    { user      :: String
    , password  :: String
    , host      :: String
    , port      :: Int
    , database  :: String
    , poolsize  :: Int
    } deriving (Show, Generic)

instance FromJSON Database

mkConnStr :: Database -> IO C.ByteString
mkConnStr s = return $ C.pack $ "host=" ++ host s ++
                                " dbname=" ++ database s ++
                                " user=" ++ user s ++
                                " password=" ++ password s ++
                                " port=" ++ show (port s)

loadYaml :: String -> IO (M.HashMap Text Value)
loadYaml fp = do
    mval <- decodeFile fp
    case mval of
        Nothing  -> error $ "Invalid YAML file: " ++ show fp
        Just obj -> return obj

parseYaml :: FromJSON a => Text -> M.HashMap Text Value -> a
parseYaml key hm =
    case M.lookup key hm of
        Just val -> case fromJSON val of
                        Success s -> s
                        Error err -> error $ "Falied to parse "
                                           ++ T.unpack key ++ ": " ++ show err
        Nothing  -> error $ "Failed to load " ++ T.unpack key
                                              ++ " from config file"

main :: IO ()
main = do
    config <- loadYaml "config/postgresql.yml"
    let db = parseYaml "Production" config :: Database
    connStr <- mkConnStr db
    res <- simpleHttp rurl
    let cursor = fromDocument $ parseLBS res
        tids = take 5 (parseTids cursor)
    withPostgresqlPool connStr (poolsize db) $ \pool ->
        flip runSqlPersistMPool pool $ do
            runMigration migrateAll

            forM_ tids $ \tid -> do
                mtalk <- getBy (UniqueTalk tid)
                when (isNothing mtalk) $ do
                    talk' <- liftIO $ queryTalk tid
                    case talk' of
                        Nothing -> return ()
                        Just talk -> do
                            dbtalk <- liftIO $ marshal talk
                            void $ insertUnique dbtalk
  where
    rurl = "http://feeds.feedburner.com/tedtalks_video"
    -- 105 tids
    parseTids cur = map (read . T.unpack) $ cur $// element "jwplayer:talkId"
                                                &// content
