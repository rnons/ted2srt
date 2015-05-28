{-# LANGUAGE OverloadedStrings #-}
-- Test cases for Web.TED module
import Data.Maybe (fromJust)
import Data.Text (Text)
import Prelude hiding (id)
import Test.HUnit
import Test.Hspec

import Web.TED

main :: IO ()
main = do
    let uri = "http://www.ted.com/talks/francis_collins_we_need_better_drugs_now.html"
    tid <- getTalkId uri
    talk <- queryTalk $ fromJust tid
    (mediaSlug, pad) <- getSlugAndPad uri
    hspec $ spec talk mediaSlug pad

spec :: Maybe Talk -> Text -> Double -> Spec
spec talk mediaSlug pad =
    describe "Ted.hs tests" $ do
        it "talk id" $
            fmap id talk  @?= Just 1696
        it "talk title" $
            fmap name talk @?= Just "Francis Collins: We need better drugs -- now"

        it "available subtitles" $ do
            let srtlist = fmap talkLanguages talk
            fmap length srtlist @?= Just 26

        it "mediaSlug" $
            mediaSlug @?= "FrancisCollins_2012P"

        it "mediaPad" $
            pad @?= 15330.0
