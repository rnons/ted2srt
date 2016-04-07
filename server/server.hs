import           Database.Redis
import           LoadEnv (loadEnv)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Servant (serve)
import           System.Environment (getEnv)

import ReTed.API (tedApi, tedServer)


app :: Connection -> Application
app = logStdout . serve tedApi . tedServer

main :: IO ()
main = do
    loadEnv
    port <- read <$> getEnv "PORT"
    redisPort <- read <$> getEnv "REDIS_PORT"
    conn <- connect defaultConnectInfo
        { connectPort = PortNumber $ fromIntegral redisPort }
    run port $ app conn
