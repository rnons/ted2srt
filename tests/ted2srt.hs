{-# LANGUAGE OverloadedStrings #-}

import Yesod.Test
import Test.Hspec (hspec)
import Foundation
import Settings

main :: IO ()
main = do
    s <- staticSite
    hspec $ yesodSpec (Ted s) homeSpecs

type Specs = YesodSpec Ted

homeSpecs :: Specs
homeSpecs = 
    ydescribe "web page tests" $ do
        yit "loads the index" $ do
            get HomeR
            statusIs 200
            htmlAllContain "#search_button" "submit"
            htmlAllContain "#search_input" "q"

        yit "lookup available subtitles" $ do
            get (HomeR, [ ("_hasdata", "")
                        , ("q", "http://www.ted.com/talks/ken_robinson_says_schools_kill_creativity.html")
                        ])
            statusIs 303

        yit "get talks" $ do
            get $ TalksR "ken_robinson_says_schools_kill_creativity.html"
            statusIs 200
            htmlCount "li" 58 
            htmlAllContain "#download" "Download"
