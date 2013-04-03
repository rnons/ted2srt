{-# LANGUAGE OverloadedStrings #-}

import Network.Wai.Handler.Warp (Connection)
import Yesod.Dispatch
import Yesod.Test
import Foundation
import Settings

main :: IO ()
main = do
    s <- staticSite
    app <- toWaiAppPlain $ Ted s
    runTests app undefined homeSpecs

type Specs = SpecsConn Connection

homeSpecs :: Specs
homeSpecs = 
    describe "web page tests" $ do
        it "loads the index" $ do
            get_ "/"
            statusIs 200
            htmlAllContain "#search_button" "submit"
            htmlAllContain "#search_input" "q"

        it "lookup available subtitles" $ do
            get_ "/?q=http://www.ted.com/talks/francis_collins_we_need_better_drugs_now.html"
            statusIs 200
            htmlCount "li" 6
            htmlAllContain "#download" "Download"
