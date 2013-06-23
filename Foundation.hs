{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Foundation where

import qualified Data.Text as T
import Filesystem.Path.CurrentOS
import System.Directory
import Text.Jasmine (minifym)
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
             talk <- liftIO $ getTalk q'
             if Prelude.null $ srtLang talk
                then redirect HomeR
                else do
                     defaultLayout $ do
                         setTitle $ toHtml $ title talk
                         addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
                         $(widgetFile "result")
         _       -> 
             defaultLayout $ do
                 setTitle "Ted2srt: Subtitles worth spreading"
                 toWidgetHead [hamlet| <meta name=description content="Look up available subtitles of TED talks, download srt of one or two languages. TED演讲双语字幕下载。">|]
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

$(staticFiles staticDir)
