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
spec talk =
    describe "Ted.hs tests" $ do
        it "talk id" $
            fmap tid talk  @?= Just "1696"
        it "talk title" $
            fmap title talk @?= Just "Francis Collins: We need better drugs -- now"
            
        it "available subtitles" $ do
            let srtlist = fmap subLang talk
            fmap length srtlist @?= Just 25

        it "mediaSlug" $
            fmap subName talk @?= Just "FrancisCollins_2012P"

        it "mediaPad" $
            fmap subLag talk @?= Just 15330.0
