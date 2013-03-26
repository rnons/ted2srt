{-# LANGUAGE DeriveDataTypeable #-}
module Ted where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.Generic as G
import Data.Array
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (isSpace)
import Data.Data
import Data.Maybe
import Network.HTTP
import System.Directory
import System.IO
import System.Process
import Text.Printf
import Text.Regex.Posix ((=~))
import Text.Regex.PCRE hiding ((=~))

data Caption = Caption
    { captions :: [Item]
    } deriving (Data, Typeable, Show)

data Item = Item
    { duration          :: Int
    , content           :: String
    , startOfParagraph  :: Bool
    , startTime         :: Int
    } deriving (Data, Typeable, Show)

rstrip = reverse . dropWhile isSpace . reverse

tedPageContent uri = 
    catch ((simpleHTTP $ getRequest $ rstrip uri) >>= getResponseBody)
          (\e -> do print (e :: SomeException)
                    return "")

--html2srt :: String -> [(String, String)]
html2srt body = do
    let reg = makeRegexOpts compDotAll defaultExecOpt "<select name=\"languageCode\"(.+?)</select>"
    case matchOnceText reg body of
         Just (_, m, _) -> do
            let (mt, _) =  m ! 1
            let srt = mt =~ "<option value=\"([^\"]+)\"[^>]*>([^<]+)</option>" :: [[String]]
            let srtlist = map (\xs -> (xs!!1, xs!!2)) srt
            return $ Just srtlist
         _              -> return Nothing

getTid body = do
    let pat = "data-id=\"([^ ]*)\""
    let r = body =~ pat :: [[String]]
    last $ last r

getTitle body = do
    let pat = "<span id=\"altHeadline\" >(.+)</span>"
        r = body =~ pat :: [[String]]
    last $ last r

mediaPad = 15330    -- time lag in miliseconds

toSrt :: String -> [String] -> IO (Maybe String)
toSrt tid [lang] = oneSrt tid lang
toSrt tid (s1:s2:_) = do
    pwd <- getCurrentDirectory
    let path = pwd ++ "/srt/" ++ tid ++ "." ++ s1 ++ "." ++ s2 ++ ".srt"
    print path
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
    print cached
    if cached 
       then return $ Just path
       else do
           let url = "http://www.ted.com/talks/subtitles/id/" 
                   ++ tid ++ "/lang/" ++ lang
           rsp <- simpleHTTP $ getRequest url
           json <- getResponseBody rsp
           print json
           let res = G.decode $ C.pack json :: Maybe Caption
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

