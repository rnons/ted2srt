{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (decodeStrict)
import           Data.Maybe (catMaybes, mapMaybe)
import           Database.Redis hiding (decode)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant

import Handler.Util

type TedApi =
    "newest" :> Get '[JSON] [RedisTalk]

getNewst :: Server TedApi
getNewst = do
    conn <- liftIO $ connect defaultConnectInfo
    emtalks <- liftIO $ runRedis conn $ do
        elatest <- lrange "latest" 0 4
        mget $ either (const []) id elatest
    let talks = mapMaybe decodeStrict $ catMaybes $
                    either (const [Nothing]) id emtalks
    return talks


tedApi :: Proxy TedApi
tedApi = Proxy

tedServer :: Server TedApi
tedServer = getNewst

app :: Application
app = serve tedApi tedServer

main :: IO ()
main = run 3001 app
