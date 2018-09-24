-- | TED API module
-- Documented at <http://developer.ted.com/io-docs>

module Web.TED.API
  ( getTalkTranscript
  ) where

import           Data.Aeson           (eitherDecode)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.HTTP.Conduit (simpleHttp)
import           RIO

import           Web.TED.Types


getTalkTranscript :: Int -> Text -> IO Text
getTalkTranscript talkId language = do
    res <- simpleHttp rurl
    case eitherDecode res of
        Right r -> return $ transcriptToText r
        Left er -> error er
  where
    rurl = "https://www.ted.com/talks/" <> show talkId <> "/transcript.json?language=" <> T.unpack language
