module ReTed.ApiSpec where

import           Data.Aeson (Value, decode)
import           Network.Wai (Application)
import           Network.Wai.Test (SResponse(..))
import           Servant (serve)
import           Test.Hspec
import           Test.Hspec.Wai

import           ReTed.API
import           ReTed.Config (Config, getConfig)
import           ReTed.TestUtils (resetDb)
import           ReTed.Models.Talk (Talk)


app :: IO Application
app = (serve tedApi . tedServer) <$> getConfig

spec :: Spec
spec = before resetDb $ with app $
    describe "true" $ do
        it "GET /talks should responds with 200" $
            get "/talks" `shouldRespondWith` 200

        it "GET /talks should responds with five talks" $ do
            res <- get "/talks"
            let v = decode $ simpleBody res :: Maybe [Value]
            liftIO $ (length <$> v) `shouldBe` Just 5
