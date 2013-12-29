{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (id)
import Test.HUnit
import Test.Hspec

import Web.TED

main :: IO ()
main = do
    let uri = "http://www.ted.com/talks/francis_collins_we_need_better_drugs_now.html"
    talk <- getTalk uri
    hspec $ spec talk

spec :: Maybe SubTalk -> Spec
spec talk =
    describe "Ted.hs tests" $ do
        it "talk id" $
            fmap id talk  @?= Just 1696
        it "talk title" $
            fmap name talk @?= Just "Francis Collins: We need better drugs -- now"

        it "available subtitles" $ do
            let srtlist = fmap languages talk
            fmap length srtlist @?= Just 27

        it "mediaSlug" $
            fmap subSlug talk @?= Just "FrancisCollins_2012P"

        it "mediaPad" $
            fmap subLag talk @?= Just 15330.0
