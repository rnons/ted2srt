{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (forM_, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (encode)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import           Database.Redis
import           Network.HTTP.Conduit (simpleHttp)
import           Text.HTML.DOM (parseLBS)
import           Text.XML.Cursor

import Handler.Util (marshal)
import Web.TED (queryTalk)


main :: IO ()
main = do
    res <- simpleHttp rurl
    conn <- connect defaultConnectInfo
    let cursor = fromDocument $ parseLBS res
        tids = take limit (parseTids cursor)

    runRedis conn $ do
        del [key]
        rpush key tids
    forM_ tids $ \tid -> do
        mtalk <- runRedis conn $ get tid
        case mtalk of
            Right (Just _) -> return ()
            _                 -> do
                talk' <- liftIO $ queryTalk $ read $ C.unpack tid
                case talk' of
                    Nothing -> return ()
                    Just talk -> do
                        dbtalk <- liftIO $ marshal talk
                        void $ runRedis conn $ set tid (L.toStrict $ encode dbtalk)
  where
    key = "latest"
    limit = 5
    rurl = "http://feeds.feedburner.com/tedtalks_video"
    -- 105 tids
    parseTids cur = map (C.pack . T.unpack) $ cur $// element "jwplayer:talkId"
                                                  &// content
