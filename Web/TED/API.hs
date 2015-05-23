-- | TED API module
-- Documented at <http://developer.ted.com/io-docs>

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.TED.API
  ( Talk (..)
  , SearchTalk (..)
  , queryTalk
  , searchTalk
  , talkImg
  , talkHasAudio
  ) where

import qualified Control.Exception as E
import           Data.Aeson
import           Data.Aeson.Types (defaultOptions, Options(..))
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit (simpleHttp)
import           Network.HTTP.Types (urlEncode)

import Web.TED.Types


-- | Response of https://api.ted.com/v1/talks/:id.json
data QueryResponse = QueryResponse
    { talk          :: Talk
    } deriving (Generic, Show)
instance FromJSON QueryResponse

-- | Response of https://api.ted.com/v1/search.json
data SearchResponse = SearchResponse
    { results       :: [SearchResult]
    } deriving (Generic, Show)
instance FromJSON SearchResponse

data SearchResult = SearchResult
    { _talk       :: SearchTalk
    } deriving (Generic, Show)
instance FromJSON SearchResult where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

-- | Return Nothing for talks hosted externally (youtube, vimeo), e.g. 720.
-- They have no downloadable subtitles and will fail getSlugAndPad.
queryTalk :: Int -> IO (Maybe Talk)
queryTalk tid = E.catch
    (do
        res <- simpleHttp rurl
        case eitherDecode res of
            Right r -> return $ Just $ talk r
            Left er -> error er)
    (\e -> print (e :: E.SomeException) >> return Nothing)
  where
    rurl = "http://api.ted.com/v1/talks/" ++ show tid ++
           ".json?external=false&podcasts=true&api-key=2a9uggd876y5qua7ydghfzrq"

-- | "languages": { "en": { "name": "English", "native": true } }
-- talkLanguages :: Talk -> [(Text, Text)]
-- talkLanguages t =
--     case languages t of
--         Just (Object langs) ->
--             let langCode = HM.keys langs
--                 langName = map ((\(String str) -> str) . (\(Object hm) -> hm HM.! "name"))
--                                (HM.elems langs)
--             in  sort $ zip langName langCode
--         _                   -> []

-- | Whether "audio-podcast" field is present
talkHasAudio :: Talk -> Bool
talkHasAudio t =
    case media t of
        Object ms -> isJust $ HM.lookup "internal" ms >>=
                            \(Object im) -> HM.lookup "audio-podcast" im
        _         -> False

-- | "images": { ["image": { "size": , "url": }] }
talkImg :: Talk -> Text
talkImg t = url $ image (images t !! 2)

searchTalk :: Text -> IO [SearchTalk]
searchTalk q = do
    res <- simpleHttp rurl
    case eitherDecode res of
        Right r -> return $ map _talk (results r)
        Left er -> error er
  where
    query = B8.unpack $ urlEncode True $ B8.pack $ T.unpack q
    rurl = "http://api.ted.com/v1/search.json?q=" <> query <>
           "&categories=talks&api-key=2a9uggd876y5qua7ydghfzrq"

