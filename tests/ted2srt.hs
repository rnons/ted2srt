{-# LANGUAGE OverloadedStrings #-}
-- Test cases for Foundation.hs
import Database.Persist (loadConfig, applyEnv, createPoolConfig)
import Database.Persist.Postgresql (PostgresConf)
import Test.Hspec (hspec)
import Yesod.Default.Config (withYamlEnvironment, DefaultEnv(..))
import Yesod.Test
import Foundation
import Settings

main :: IO ()
main = do
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" Production
              loadConfig >>= applyEnv
    p <- createPoolConfig (dbconf :: PostgresConf)
    hspec $ yesodSpec (Ted s p dbconf) homeSpecs

type Specs = YesodSpec Ted

homeSpecs :: Specs
homeSpecs = 
    ydescribe "web page tests" $ do
        yit "loads the index" $ do
            get HomeR
            statusIs 200
            htmlAllContain "#search_button" "submit"
            htmlAllContain "#search_input" "q"

        yit "get talks" $ do
            get $ TalksR "ken_robinson_says_schools_kill_creativity.html"
            statusIs 200
            htmlCount "#main li" 58 
            htmlCount "#sidepane li" 8 

        yit "test search" $ do
            get $ SearchR "design"
            statusIs 200

        -- 302 is not helpful here
        yit "lookup available subtitles" $ do
            get (HomeR, [ ("_hasdata", "")
                        , ("q", "http://www.ted.com/talks/ken_robinson_says_schools_kill_creativity.html")
                        ])
            statusIs 302

        yit "test new.ted.com" $ do
            get (HomeR, [ ("_hasdata", "")
                        , ("q", "http://new3.ted.com/talks/ken_robinson_says_schools_kill_creativity")
                        ])
            statusIs 302
