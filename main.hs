{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

import Control.Applicative
import Data.Text
import Text.Cassius
import Text.Julius
import Yesod
import System.Directory
import Filesystem.Path.CurrentOS

import Ted

data Ted = Ted

data Search = Search
    { url :: Text
    }

mkYesod "Ted" [parseRoutes|
/ HomeR GET
/download DownloadR GET
|]

instance Yesod Ted

getHomeR :: Handler RepHtml
getHomeR = do
    q <- lookupGetParam "q"
    case q of
         Just q' -> do
             body <- liftIO $ tedPageContent (unpack q')
             res <- html2srt body
             let tid = getTid body
                 title = getTitle body
             case res of
                  Just srtlist -> 
                     defaultLayout $ do
                         setTitle $ toHtml title
                         addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
                         $(whamletFile "templates/result.hamlet")
                         toWidget $(cassiusFileReload "templates/result.cassius")
                         toWidget $(juliusFileReload "templates/result.julius")
                  _            -> redirect HomeR
         _       -> do
             defaultLayout $ do
                 $(whamletFile "templates/homepage.hamlet")
                 toWidget $(cassiusFileReload "templates/homepage.cassius")

getDownloadR :: Handler RepPlain
getDownloadR = do
    pwd <- liftIO $ getCurrentDirectory
    t  <- lookupGetParam "tid"
    l  <- lookupGetParams "lang"
    liftIO $ print l
    case (t, l) of
         (Just t', l') -> do
             path <- liftIO $ toSrt (unpack t') (Prelude.map unpack l')
             case path of
                  Just p -> do
                    liftIO $ print p
                    let name = filename $ decodeString p
                    setHeader "Content-Disposition" $ pack ("attachment; filename=" ++ encodeString name)
                    sendFile typePlain p
                  _      -> redirect HomeR
         _                  -> redirect HomeR 

main :: IO ()
main = warpDebug 3000 Ted
