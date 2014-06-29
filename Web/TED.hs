{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Web.TED
  ( Subtitle (..)
  , FileType (..)
  , Item (..)
  , toSub
  , responseSize
  -- * Re-exports
  , module Web.TED.API
  , module Web.TED.TalkPage
  ) where

import           Control.Exception as E
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as B8
import           Data.Conduit (($$+-))
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           qualified Data.Text as T
import           qualified Data.Text.IO as T
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit hiding (path)
import           System.Directory
import           System.IO
import           Text.HTML.DOM (parseLBS)
import           Text.Printf
import           Text.XML.Cursor (attributeIs, element, fromDocument, ($//), (&//))
import qualified Text.XML.Cursor as XC

import Web.TED.API
import Web.TED.TalkPage


data Caption = Caption
    { captions :: [Item]
    } deriving (Generic, Show)
instance FromJSON Caption

data Item = Item
    { duration          :: Int
    , content           :: Text
    , startOfParagraph  :: Bool
    , startTime         :: Int
    } deriving (Generic, Show)
instance FromJSON Item

data FileType = SRT | VTT | TXT | LRC
    deriving (Show, Eq)

data Subtitle = Subtitle
    { talkId            :: Text
    , talkslug          :: Text
    , language          :: [Text]
    , filename           :: Text
    , timeLag           :: Double
    , filetype          :: FileType
    } deriving Show

availableLanguages :: [Text]
availableLanguages =
    [ "af", "sq", "arq", "am", "ar", "hy", "as", "ast", "az", "eu", "be", "bn"
    , "bi", "bs", "bg", "my", "ca", "ceb", "zh-cn", "zh-tw", "zh", "ht", "hr"
    , "cs", "da", "nl", "en", "eo", "et", "fil", "fi", "fr", "fr-ca", "gl"
    , "ka", "de", "el", "gu", "ha", "he", "hi", "hu", "hup", "is", "id", "inh"
    , "ga", "it", "ja", "kn", "kk", "km", "tlh", "ko", "ku", "ky", "lo", "ltg"
    , "la", "lv", "lt", "lb", "rup", "mk", "mg", "ms", "ml", "mt", "mr", "mn"
    , "srp", "ne", "nb", "nn", "oc", "fa", "pl", "pt", "pt-br", "ro", "ru"
    , "sr", "sh", "szl", "si", "sk", "sl", "so", "es", "sw", "sv", "tl", "tg"
    , "ta", "tt", "te", "th", "bo", "tr", "uk", "ur", "ug", "uz", "vi"
    ]

toSub :: Subtitle -> IO (Maybe FilePath)
toSub sub
    | any (`notElem` availableLanguages) lang = return Nothing
    | filetype sub == LRC = oneLrc sub
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
                         mergeFile p1' p2' path
                         return $ Just path
                     _                    -> return Nothing
    | otherwise = return Nothing
  where
    lang = language sub
    (dir, suffix, func) = case filetype sub of
        SRT -> ("/static/srt/", ".srt", oneSub)
        VTT -> ("/static/vtt/", ".vtt", oneSub)
        TXT -> ("/static/txt/", ".txt", oneTxt)
        LRC -> ("/static/lrc/", ".lrc", oneLrc)

oneSub :: Subtitle -> IO (Maybe FilePath)
oneSub sub = do
    path <- subtitlePath sub
    cached <- doesFileExist path
    if cached
       then return $ Just path
       else do
           let rurl = T.unpack $ "http://www.ted.com/talks/subtitles/id/"
                     <> talkId sub <> "/lang/" <> head (language sub)
           E.catch (do
               res <- simpleHttp rurl
               let decoded = decode res :: Maybe Caption
               case decoded of
                    Just r -> do
                        h <- if filetype sub == VTT
                                 then do
                                     h <- openFile path WriteMode
                                     hPutStrLn h "WEBVTT\n"
                                     return h
                                 else do
                                     -- Prepend the UTF-8 byte order mark
                                     -- to do Windows user a favor.
                                     withBinaryFile path WriteMode $ \h ->
                                         hPutStr h "\xef\xbb\xbf"
                                     openFile path AppendMode
                        forM_ (zip (captions r) [1,2..]) (ppr h)
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
        hPrintf h fmt (i::Int) sh sm ss sms eh em es ems (T.unpack $ content c)

oneTxt :: Subtitle -> IO (Maybe FilePath)
oneTxt sub = do
    path <- subtitlePath sub
    cached <- doesFileExist path
    if cached
       then return $ Just path
       else do
           let rurl = T.unpack $ "http://www.ted.com/talks/" <> talkslug sub
                               <> "/transcript?lang=" <> head (language sub)
           res <- simpleHttp rurl
           let cursor = fromDocument $ parseLBS res
               con = cursor $// element "span"
                            >=> attributeIs "class" "talk-transcript__para__text"
                            &// XC.content
               txt = scanl1 (\x1 x2 -> if x2 == "\n" then if x1 == " "
                                             then "\n\n" else " " else x2) con
           -- Prepend the UTF-8 byte order mark to do Windows user a favor.
           withBinaryFile path WriteMode $ \h ->
               hPutStr h "\xef\xbb\xbf"
           T.appendFile path $ T.concat txt
           return $ Just path

oneLrc :: Subtitle -> IO (Maybe FilePath)
oneLrc sub = do
    path <- subtitlePath sub
    cached <- doesFileExist path
    if cached
       then return $ Just path
       else do
           let rurl = T.unpack $ "http://www.ted.com/talks/subtitles/id/"
                     <> talkId sub <> "/lang/en"
           E.catch (do
               res <- simpleHttp rurl
               let decoded = decode res :: Maybe Caption
               case decoded of
                    Just r -> do
                        h <- openFile path WriteMode
                        forM_ (captions r) (ppr h)
                        hClose h
                        return $ Just path
                    Nothing ->
                        return Nothing)
               (\e -> do print (e :: E.SomeException)
                         return Nothing)
  where
    fmt = "[%02d:%02d.%02d]%s\n"
    ppr h c = do
        let st = startTime c + floor (timeLag sub) + 3000
            sm = st `div` 1000 `mod` 3600 `div` 60
            ss = st `div` 1000 `mod` 60
            sms = st `mod` 100
        hPrintf h fmt sm ss sms (T.unpack $ content c)

mergeFile :: FilePath -> FilePath -> FilePath -> IO ()
mergeFile p1 p2 path = do
    c1 <- T.readFile p1
    c2 <- T.readFile p2
    let merged = T.unlines $ merge (T.lines c1) (T.lines c2)
    T.writeFile path merged

-- | Merge srt files of two language line by line. However,
-- one line in srt_1 may correspond to two lines in srt_2, or vice versa.
merge :: [Text] -> [Text] -> [Text]
merge (a:as) (b:bs)
    | a == b    = a : merge as bs
    | a == ""   = b : merge (a:as) bs
    | b == ""   = a : merge as (b:bs)
    | otherwise = a : b : merge as bs
merge _      _      = []

-- Construct file path according to filetype.
subtitlePath :: Subtitle -> IO FilePath
subtitlePath sub =
    case filetype sub of
        SRT -> path ("/static/srt/", ".srt")
        VTT -> pathTr ("/static/vtt/", ".vtt")
        TXT -> path ("/static/txt/", ".txt")
        LRC -> pathEn ("/static/lrc/", ".lrc")
  where
    path = if head (language sub) == "en" then pathEn else pathTr
    pathTr (dir, suffix) = do
        pwd <- getCurrentDirectory
        return $ T.unpack $ T.concat [ T.pack pwd
                                     , dir
                                     , filename sub
                                     , "."
                                     , head $ language sub
                                     , suffix
                                     ]
    pathEn (dir, suffix) = do
        pwd <- getCurrentDirectory
        return $ T.unpack $ T.concat [ T.pack pwd
                                     , dir
                                     , filename sub
                                     , suffix
                                     ]

responseSize :: Text -> IO Float
responseSize rurl = E.catch
    (do req <- parseUrl $ T.unpack rurl
        filesize <- withManager $ \manager -> do
            res <- http req manager
            let hdrs = responseHeaders res
            responseBody res $$+- return ()
            return $ HM.fromList hdrs HM.! "Content-Length"
        return $ read (B8.unpack filesize) / 1024 / 1024)
    (\e -> do print (e :: E.SomeException)
              return 0)
