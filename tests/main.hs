import Control.Monad.IO.Class
import Test.HUnit
import Test.Hspec

import Ted

main :: IO ()
main = do
    let uri = "http://www.ted.com/talks/francis_collins_we_need_better_drugs_now.html"
    body <- tedPageContent uri
    hspec $ spec body

spec :: String -> Spec
spec body = do 
    describe "Ted.hs tests" $ do
        it "talk id" $
            getTid body @?= "1696"
        it "talk title" $
            getTitle body @?= "Francis Collins: We need better drugs -- now"
            
        it "available subtitles" $ do
            srtlist <- html2srt body
            fmap length srtlist @?= Just 2
