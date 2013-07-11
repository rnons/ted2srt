{-# LANGUAGE OverloadedStrings #-}

import Yesod.Test
import Test.Hspec (hspec)
import Foundation
import Settings

main :: IO ()
main = do
    s <- staticSite
    hspec $ do
        yesodSpec (Ted s) homeSpecs

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
            let reqBuilder = addGetParam "q" "http://www.ted.com/talks/francis_collins_we_need_better_drugs_now.html"
            request reqBuilder
            statusIs 200
            htmlCount "li" 23 
            htmlAllContain "#download" "Download"
