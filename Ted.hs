{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Ted where

import Control.Monad
import Data.Aeson
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Data
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Conduit
import System.Directory
import System.IO
import System.Process
import Text.HTML.DOM (parseLBS)
import Text.Printf
import Text.Regex.Posix ((=~))
import Text.XML.Cursor (attribute, attributeIs, element, fromDocument,
                        ($//), (&|), (&//), (>=>))
import qualified Text.XML.Cursor as XC
import Control.Monad.IO.Class (liftIO)

data Caption = Caption
    { captions :: [Item]
    } deriving (Data, Typeable, Show)

data Item = Item
    { duration          :: Int
    , content           :: String
    , startOfParagraph  :: Bool
    , startTime         :: Int
    } deriving (Data, Typeable, Show)

data Talk = Talk 
    { tid               :: Text
    , title             :: Text
    , srtLang              :: [(Text, Text)]
    , srtName          :: String
    , srtLag           :: Double
    } deriving Show

getTalk :: Text -> IO Talk
getTalk uri = do
    body <- simpleHttp $ T.unpack uri
{-
    request <- parseUrl $ T.unpack uri
    body <- responseBody $ withManager $ httpLbs request
    body <- liftIO $ withManager $ \manager -> do
        response <- http request manager
        responseBody response
-}
    let cursor = fromDocument $ parseLBS body
    return Talk { tid = talkIdTitle cursor "data-id"
               , title = talkIdTitle cursor "data-title"
               , srtLang = languageCodes cursor
               , srtName = mediaSlug body
               , srtLag = mediaPad body
               }

talkIdTitle cursor name = attr name
  where
    cur = head $ cursor $// element "div" >=> attributeIs "id" "share_and_save"
    attr name = head $ attribute name cur

{- List in the form of [("English", "en")] -}
languageCodes cursor = zip lang code
  where
    lang = cursor $// element "select" 
                  >=> attributeIs "name" "languageCode" 
                  &// element "option" &// XC.content
    code = concat $ cursor $// element "select" 
                           >=> attributeIs "name" "languageCode" 
                           &// element "option"
                           &| attribute "value"

{- File name when saved to local. -}
mediaSlug :: L8.ByteString -> String
mediaSlug body = last $ last r
  where
    pat = "mediaSlug\":\"([^\"]+)\"" :: String
    r = (L8.unpack body) =~ pat :: [[String]]

{- Time lag in miliseconds. -}
mediaPad :: L8.ByteString -> Double
mediaPad body = read t * 1000.0
  where
    pat = "mediaPad\":(.+)}" :: String
    r = (L8.unpack body) =~ pat :: [[String]]
    t = last $ last r

toSrt :: Text -> [Text] -> Text -> Int -> IO (Maybe String)
toSrt tid [lang] fname lag = oneSrt tid lang fname lag
toSrt tid (s1:s2:_) fname lag = do
    pwd <- getCurrentDirectory
    let path = T.unpack $ T.concat [ T.pack pwd
                                   , "/srt/"
                                   , fname
                                   , "."
                                   , s1
                                   , "."
                                   , s2
                                   , ".srt"
                                   ]
    cached <- doesFileExist path
    if cached 
       then return $ Just path
       else do
            p1 <- oneSrt tid s1 fname lag
            p2 <- oneSrt tid s2 fname lag
            case (p1, p2) of
                 (Just p1', Just p2') -> do
                     c1 <- readFile p1'
                     c2 <- readFile p2'
                     let content = unlines $ merge (lines c1) (lines c2)
                     writeFile path content
                     return $ Just path
                 _                    -> return Nothing

oneSrt :: Text -> Text -> Text -> Int -> IO (Maybe String)
oneSrt tid lang fname lag = do
    pwd <- getCurrentDirectory
    let path = T.unpack $ T.concat [ T.pack pwd
                                   , "/srt/"
                                   , fname
                                   , "."
                                   , lang
                                   , ".srt"
                                   ]
    cached <- doesFileExist path
    if cached 
       then return $ Just path
       else do
           let url = "http://www.ted.com/talks/subtitles/id/" 
                   ++ T.unpack tid ++ "/lang/" ++ T.unpack lang
           json <- simpleHttp url
           let res = G.decode json :: Maybe Caption
           case res of
                Just r -> do
                    h <- openFile path WriteMode
                    forM_ (zip (captions $ fromJust res) [1,2..]) (ppr h)
                    hClose h
                    return $ Just path
                Nothing -> 
                    return Nothing
  where
    ppr h (c,i) = do
        --let st = startTime c + (read $ T.unpack lag)
        let st = startTime c + lag
            sh = st `div` 1000 `div` 3600
            sm = st `div` 1000 `mod` 3600 `div` 60
            ss = st `div` 1000 `mod` 60
            sms = st `mod` 1000
            et = st + duration c
            eh = et `div` 1000 `div` 3600
            em = et `div` 1000 `mod` 3600 `div` 60
            es = et `div` 1000 `mod` 60
            ems = et `mod` 1000
        let fmt = "%d\n%02d:%02d:%02d,%03d --> " ++
                  "%02d:%02d:%02d,%03d\n%s\n\n"
        hPrintf h fmt (i::Int) sh sm ss sms eh em es ems (content c)

-- | Merge srt files of two language line by line. However,
-- one line in srt_1 may correspond to two lines in srt_2, or vice versa.
merge :: [String] -> [String] -> [String]
merge (a:as) (b:bs) 
    | a == b    = a : merge as bs
    | a == ""   = b : merge (a:as) bs
    | b == ""   = a : merge as (b:bs)
    | otherwise = a : b : merge as bs
merge _      _      = []

