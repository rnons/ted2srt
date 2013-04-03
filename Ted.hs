{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Ted where

import Control.Monad
import Data.Aeson
import qualified Data.Aeson.Generic as G
import Data.Data
import Data.Maybe
import Network.HTTP.Conduit (simpleHttp)
import System.Directory
import System.IO
import System.Process
import Text.HTML.DOM (parseLBS)
import Text.Printf
import Text.XML.Cursor (attribute, attributeIs, element, fromDocument,
                        ($//), (&|), (&//), (>=>))
import qualified Text.XML.Cursor as XC

data Caption = Caption
    { captions :: [Item]
    } deriving (Data, Typeable, Show)

data Item = Item
    { duration          :: Int
    , content :: String
    , startOfParagraph  :: Bool
    , startTime         :: Int
    } deriving (Data, Typeable, Show)

getCursor uri = do
    lbs <- simpleHttp uri
    return $ fromDocument $ parseLBS lbs

talkIdTitle cursor = (attr "data-id", attr "data-title")
  where
    cur = head $ cursor $// element "div" >=> attributeIs "id" "share_and_save"
    attr name = head $ attribute name cur

languageCodeList cursor = zip lang code
  where
    lang = cursor $// element "select" 
                  >=> attributeIs "name" "languageCode" 
                  &// element "option" &// XC.content
    code = concat $ cursor $// element "select" 
                           >=> attributeIs "name" "languageCode" 
                           &// element "option"
                           &| attribute "value"

mediaPad = 15330    -- time lag in miliseconds

toSrt :: String -> [String] -> IO (Maybe String)
toSrt tid [lang] = oneSrt tid lang
toSrt tid (s1:s2:_) = do
    pwd <- getCurrentDirectory
    let path = pwd ++ "/srt/" ++ tid ++ "." ++ s1 ++ "." ++ s2 ++ ".srt"
    cached <- doesFileExist path
    if cached 
       then return $ Just path
       else do
            p1 <- oneSrt tid s1
            p2 <- oneSrt tid s2
            case (p1, p2) of
                 (Just p1', Just p2') -> do
                     c1 <- readFile p1'
                     c2 <- readFile p2'
                     let content = unlines $ merge (lines c1) (lines c2)
                     writeFile path content
                     return $ Just path
                 _                    -> return Nothing

oneSrt :: String -> String -> IO (Maybe String)
oneSrt tid lang = do
    pwd <- getCurrentDirectory
    let path = pwd ++ "/srt/" ++ tid ++ "." ++ lang ++ ".srt"
    cached <- doesFileExist path
    if cached 
       then return $ Just path
       else do
           let url = "http://www.ted.com/talks/subtitles/id/" 
                   ++ tid ++ "/lang/" ++ lang
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
        let st = startTime c + mediaPad
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

