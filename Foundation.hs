{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Foundation where

import Data.Maybe (maybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Filesystem.Path.CurrentOS
import System.Directory
import Text.Jasmine (minifym)
import Text.Julius (rawJS)
import Yesod
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Static

import Settings
import Ted

data Ted = Ted
    { getStatic :: Static
    }

mkYesod "Ted" [parseRoutes|
/static StaticR Static getStatic
/favicon.ico FaviconR GET
/ HomeR GET
/download DownloadR GET
/size SizeR POST
|]

instance Yesod Ted where
    addStaticContent = 
        addStaticContentExternal minifym genFileName staticDir (StaticR . flip StaticRoute [])
      where
        genFileName = base64md5

getFaviconR :: MonadHandler m => m ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

getHomeR :: Handler Html
getHomeR = do
    q <- lookupGetParam "q"
    case q of
         Just q' -> do
             talk' <- liftIO $ getTalk q'
             case talk' of
                  Just talk -> defaultLayout $ do
                      let prefix = "http://download.ted.com/talks/" <> srtName talk 
                          --audio = T.unpack (prefix <> ".mp3")
                          audio = prefix <> ".mp3"
                          v1500k = prefix <> "-1500k.mp4"
                          v950k = prefix <> "-950k.mp4"
                          v600k = prefix <> "-600k.mp4"
                          v450k = prefix <> "-450k.mp4"
                          v320k = prefix <> "-320k.mp4"
                          v180k = prefix <> "-180k.mp4"
                          v64k = prefix <> "-64k.mp4"
                      setTitle $ toHtml $ title talk
                      addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
                      $(widgetFile "result")
                  _          -> redirect HomeR
         _       -> 
             defaultLayout $ do
                 setTitle "Ted2srt: Subtitles worth spreading"
                 toWidgetHead [hamlet| <meta name=description content="Download subtitles of TED talks. Choose from a list of all avialable languages. Download TED talks subtitles as srt files. You can even download subtitles of two languages as a single srt file! TED演讲双语字幕下载。">|]
                 $(widgetFile "homepage")

getDownloadR :: Handler RepPlain
getDownloadR = do
    tid   <- lookupGetParam "tid"
    lang  <- lookupGetParams "lang"
    fname <- lookupGetParam "fname"
    lag <- lookupGetParam "lag"
    case (tid, fname, lag) of
         (Just tid', Just fname', Just lag') -> do
             let lagtime = floor $ read $ T.unpack lag'
             path <- liftIO $ toSrt tid' lang fname' lagtime
             case path of
                  Just p -> do
                    liftIO $ print p
                    -- filename "srt/foo.srt" == "foo.srt"
                    let name = filename $ decodeString p
                    addHeader "Content-Disposition" $ 
                        T.pack ("attachment; filename=" ++ encodeString name)
                    sendFile typePlain p
                  _      -> redirect HomeR
         _                  -> redirect HomeR 

postSizeR :: Handler Value
postSizeR = do
    url <- lookupPostParam "url"
    size <- lift $ maybe (return 0) responseSize url
    returnJson $ object $ ["size" .= size]

$(staticFiles staticDir)
