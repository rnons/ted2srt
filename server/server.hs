import           Database.Redis
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Servant (serve)

import ReTed.API (tedApi, tedServer)


app :: Connection -> Application
app = logStdout . serve tedApi . tedServer

main :: IO ()
main = do
    conn <- connect defaultConnectInfo { connectDatabase = 1 }
    run 3001 $ app conn
