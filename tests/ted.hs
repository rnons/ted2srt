{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import Test.HUnit
import Test.Hspec
import Text.XML.Cursor

import Ted

main :: IO ()
main = do
    let uri = "http://www.ted.com/talks/francis_collins_we_need_better_drugs_now.html"
    talk <- getTalk uri
    hspec $ spec talk

spec :: Maybe Talk -> Spec
spec talk = do 
    describe "Ted.hs tests" $ do
        it "talk id" $
            fmap tid talk  @?= Just "1696"
        it "talk title" $
            fmap title talk @?= Just "Francis Collins: We need better drugs -- now"
            
        it "available subtitles" $ do
            let srtlist = fmap srtLang talk
            fmap length srtlist @?= Just 24

        it "mediaSlug" $ do
            fmap srtName talk @?= Just "FrancisCollins_2012P"

        it "mediaPad" $ do
            fmap srtLag talk @?= Just 15330.0
