{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import Test.HUnit
import Test.Hspec
import Text.XML.Cursor

import Ted

main :: IO ()
main = do
    let uri = "http://www.ted.com/talks/francis_collins_we_need_better_drugs_now.html"
    cur <- getCursor uri
    hspec $ spec cur

spec :: Cursor -> Spec
spec cur = do 
    describe "Ted.hs tests" $ do
        let (tid, title) = talkIdTitle cur
        it "talk id" $
            tid @?= "1696"
        it "talk title" $
            title @?= "Francis Collins: We need better drugs -- now"
            
        it "available subtitles" $ do
            let srtlist = languageCodeList cur
            length srtlist @?= 7
