import           Database.Persist (loadConfig, applyEnv, createPoolConfig)
import           Database.Persist.Postgresql (PostgresConf)
import           Yesod (warp)
import           Yesod.Default.Config (withYamlEnvironment, DefaultEnv(..))

import Foundation (Ted(..))
import Settings (staticSite)

main :: IO ()
main = do
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" Production
              loadConfig >>= applyEnv
    p <- createPoolConfig (dbconf :: PostgresConf)
    warp 3000 $ Ted s p dbconf
