{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

import qualified Data.Text as T
import Filesystem.Path.CurrentOS
import System.Directory
import Text.Cassius
import Text.Jasmine (minifym)
import Text.Julius
import Yesod
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Static

import Ted
import Settings

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

getFaviconR :: GHandler s m ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

getHomeR :: Handler RepHtml
getHomeR = do
    q <- lookupGetParam "q"
    case q of
         Just q' -> do
             body <- liftIO $ tedPageContent (T.unpack q')
             res <- html2srt body
             liftIO $ print res
             case res of
                  Just srtlist -> do
                     let tid = getTid body
                         title = getTitle body
                     defaultLayout $ do
                         setTitle $ toHtml title
                         addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
                         $(whamletFile "templates/result.hamlet")
                         toWidget $(cassiusFileReload "templates/default.cassius")
                         toWidget $(juliusFileReload "templates/result.julius")
                  _            -> redirect HomeR
         _       -> 
             defaultLayout $ do
                 setTitle "Ted2srt: Subtitles worth spreading"
                 $(whamletFile "templates/homepage.hamlet")
                 toWidget $(cassiusFileReload "templates/default.cassius")

getDownloadR :: Handler RepPlain
getDownloadR = do
    pwd <- liftIO getCurrentDirectory
    tid   <- lookupGetParam "tid"
    lang  <- lookupGetParams "lang"
    title <- lookupGetParam "title"
    case (tid, lang, title) of
         (Just tid', lang', title') -> do
             path <- liftIO $ toSrt (T.unpack tid') (Prelude.map T.unpack lang')
             case path of
                  Just p -> do
                    liftIO $ print p
                    let name = filename $ decodeString p
                    setHeader "Content-Disposition" $ T.pack ("attachment; filename=" ++ encodeString name)
                    sendFile typePlain p
                  _      -> redirect HomeR
         _                  -> redirect HomeR 

$(staticFiles staticDir)

main :: IO ()
main = do
    s <- staticSite
    warpDebug 3000 $ Ted s
