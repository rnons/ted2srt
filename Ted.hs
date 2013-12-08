{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Ted where

import           Control.Exception as E
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Conduit (($$+-))
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           qualified Data.Text as T
import           qualified Data.Text.IO as T
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit
import           System.Directory
import           System.IO
import           Text.HTML.DOM (parseLBS)
import           Text.Printf
import           Text.Regex.Posix ((=~))
import           Text.XML.Cursor (attribute, attributeIs, element, fromDocument,
                                  ($//), (&|), (&//))
import qualified Text.XML.Cursor as XC

import Ted.Fallback
import Ted.Types

data Caption = Caption
    { captions :: [Item]
    } deriving (Generic, Show)

data Item = Item
    { duration          :: Int
    , content           :: String
    , startOfParagraph  :: Bool
    , startTime         :: Int
    } deriving (Generic, Show)

data FileType = SRT | VTT | TXT
    deriving (Show, Eq)

data Subtitle = Subtitle
    { talkId            :: Text
    , language          :: [Text]
    , filename           :: Text
    , timeLag           :: Double
    , filetype          :: FileType
    } deriving Show

instance FromJSON Caption
instance FromJSON Item

toSub :: Subtitle -> IO (Maybe String)
toSub sub 
    | length lang == 1 = func sub
    | length lang == 2 = do
        pwd <- getCurrentDirectory
        let (s1, s2) = (head lang, last lang)
            path = T.unpack $ T.concat [ T.pack pwd
                                       , dir
                                       , filename sub
                                       , "."
                                       , s1
                                       , "."
                                       , s2
                                       , suffix
                                       ]
        cached <- doesFileExist path
        if cached 
           then return $ Just path
           else do
                p1 <- func sub { language = [s1] }
                p2 <- func sub { language = [s2] }
                case (p1, p2) of
                     (Just p1', Just p2') -> do
                         c1 <- readFile p1'
                         c2 <- readFile p2'
                         let content = unlines $ merge (lines c1) (lines c2)
                         writeFile path content
                         return $ Just path
                     _                    -> return Nothing
    | otherwise = return Nothing
  where
    lang = language sub
    (dir, suffix, func) = case filetype sub of
        SRT -> ("/static/srt/", ".srt", oneSub)
        VTT -> ("/static/vtt/", ".vtt", oneSub)
        TXT -> ("/static/txt/", ".txt", oneTxt)

oneSub :: Subtitle -> IO (Maybe String)
oneSub sub = do
    path <- subtitlePath sub
    cached <- doesFileExist path
    if cached 
       then return $ Just path
       else do
           let url = T.unpack $ "http://www.ted.com/talks/subtitles/id/" 
                     <> talkId sub <> "/lang/" <> head (language sub)
           E.catch (do
               json <- simpleHttp url
               let res = decode json :: Maybe Caption
               case res of
                    Just r -> do
                        h <- openFile path WriteMode
                        when (filetype sub == VTT) (hPutStrLn h "WEBVTT\n")
                        forM_ (zip (captions $ fromJust res) [1,2..]) (ppr h)
                        hClose h
                        return $ Just path
                    Nothing -> 
                        return Nothing)
               (\e -> do print (e :: E.SomeException)
                         return Nothing)
  where
    fmt = if filetype sub == SRT
             then "%d\n%02d:%02d:%02d,%03d --> " ++
                  "%02d:%02d:%02d,%03d\n%s\n\n"
             else "%d\n%02d:%02d:%02d.%03d --> " ++
                  "%02d:%02d:%02d.%03d\n%s\n\n"
    ppr h (c,i) = do
        let st = startTime c + floor (timeLag sub)
            sh = st `div` 1000 `div` 3600
            sm = st `div` 1000 `mod` 3600 `div` 60
            ss = st `div` 1000 `mod` 60
            sms = st `mod` 1000
            et = st + duration c
            eh = et `div` 1000 `div` 3600
            em = et `div` 1000 `mod` 3600 `div` 60
            es = et `div` 1000 `mod` 60
            ems = et `mod` 1000
        hPrintf h fmt (i::Int) sh sm ss sms eh em es ems (content c)

oneTxt :: Subtitle -> IO (Maybe String)
oneTxt sub = do
    path <- subtitlePath sub
    cached <- doesFileExist path
    if cached 
       then return $ Just path
       else do
           let url = T.unpack $ "http://www.ted.com/talks/subtitles/id/" 
                     <> talkId sub <> "/lang/" <> head (language sub)
                     <> "/format/html"
           res <- simpleHttp url
           let cursor = fromDocument $ parseLBS res
               con = cursor $// element "p" &// XC.content
               txt = filter (`notElem` ["\n", "\n\t\t\t\t\t"]) con
               txt' = map (\t -> if t == "\n\t\t\t" then "\n\n" else t)
                          txt
           T.writeFile path $ T.concat txt'
           return $ Just path

-- | Merge srt files of two language line by line. However,
-- one line in srt_1 may correspond to two lines in srt_2, or vice versa.
merge :: [String] -> [String] -> [String]
merge (a:as) (b:bs) 
    | a == b    = a : merge as bs
    | a == ""   = b : merge (a:as) bs
    | b == ""   = a : merge as (b:bs)
    | otherwise = a : b : merge as bs
merge _      _      = []

subtitlePath :: Subtitle -> IO String
subtitlePath sub = 
    case filetype sub of
        SRT -> path ("/static/srt/", ".srt")
        VTT -> path ("/static/vtt/", ".vtt")
        TXT -> path ("/static/txt/", ".txt")
  where
    path (dir, suffix) = do
        pwd <- getCurrentDirectory
        return $ T.unpack $ T.concat [ T.pack pwd
                                     , dir
                                     , filename sub
                                     , "."
                                     , head $ language sub
                                     , suffix
                                     ]

responseSize :: Text -> IO Float
responseSize url = E.catch
    (do req <- parseUrl $ T.unpack url
        size <- withManager $ \manager -> do
            res <- http req manager
            let hdrs = responseHeaders res
            responseBody res $$+- return ()
            return $ HM.fromList hdrs HM.! "Content-Length"
        return $ read (B8.unpack size) / 1024 / 1024)
    (\e -> do print (e :: E.SomeException)
              return 0)

queryTalk :: Int -> IO Ted.Types.Talk
queryTalk tid = do
    res <- simpleHttp rurl
    case eitherDecode res of
        Right r -> return $ Ted.Types.talk r
        Left er -> error er

  where
    rurl = "https://api.ted.com/v1/talks/" ++ show tid ++
           ".json?api-key=2a9uggd876y5qua7ydghfzrq"

-- "languages": { "en": { "name": "English", "native": true } }
talkLanguages :: Ted.Types.Talk -> [(Text, Text)]
talkLanguages talk = zip langCode langName
  where
    Object langs = Ted.Types.languages talk
    langCode = HM.keys langs
    langName = map ((\(String str) -> str) . (\(Object hm) -> hm HM.! "name")) 
                   (HM.elems langs)

talkImage :: Ted.Types.Talk -> Text
talkImage talk = url $ image (images talk !! 1)

getSlugAndPad :: Text -> IO (Text, Double)
getSlugAndPad rurl = E.catch
    (do body <- simpleHttp $ T.unpack rurl
        return (mediaSlug body, mediaPad body)
    )
    (\e -> error $ show (e :: E.SomeException))
