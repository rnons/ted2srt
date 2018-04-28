import           LoadEnv                              (loadEnv)
import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Servant                              (serve)
import           System.Environment                   (getEnv)

import           Config                               (Config (..), getConfig)
import           RIO
import           Server                               (tedApi, tedServer)


app :: Config -> Application
app = logStdout . serve tedApi . tedServer

main :: IO ()
main = do
  loadEnv
  port <- read <$> getEnv "PORT"
  config <- getConfig
  run port $ app config
