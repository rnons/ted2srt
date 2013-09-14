{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Foundation where

import           Data.Maybe (maybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FS
import           System.Directory
import           Text.Blaze.Internal (Markup)
import           Text.Jasmine (minifym)
import           Text.Julius (rawJS)
import           Yesod
import           Yesod.Default.Util (addStaticContentExternal)
import           Yesod.Static

import Settings
import Settings.StaticFiles
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
/play PlayR GET
/watch WatchR GET
/talks/#Text TalksR GET
|]

instance Yesod Ted where
    addStaticContent = 
        addStaticContentExternal minifym genFileName staticDir (StaticR . flip StaticRoute [])
      where
        genFileName = base64md5

instance RenderMessage Ted FormMessage where
    renderMessage _ _ = defaultFormMessage

talkForm :: Markup -> MForm (HandlerT Ted IO) (FormResult T.Text, Widget)
talkForm = renderDivs $ areq talkUrlField ""
                                         { fsId = Just "search_input"
                                         , fsName = Just "q"
                                         , fsAttrs = [ ("placeholder", "URL")
                                                     , ("autofocus", "true")
                                                     ]
                                         } Nothing
  where
    errorMessage :: T.Text
    errorMessage = "Valid url is like this http://www.ted.com/talks/thomas_heatherwick.html"
    
    talkUrlField = check validUrl textField
    validUrl x
        | talkUrl `T.isPrefixOf` x = Right x
        | otherwise                = Left errorMessage

getFaviconR :: MonadHandler m => m ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

getHomeR :: Handler Html
getHomeR = do
    ((result, widget), enctype) <- runFormGet talkForm
    case result of
         FormSuccess q -> redirect $ TalksR $ T.drop (T.length talkUrl) q
         _       -> defaultLayout $ do
             setTitle "Ted2srt: Download TED talks with two-language subtitles | Subtitles worth spreading"
             toWidgetHead [hamlet| <meta name=description content="Choose from all available subtitle languages, download as srt file. Combine two-language subtitles in to one file. Learn some english while watching TED talks. TED演讲双语字幕下载。">|]
             $(widgetFile "homepage")

getDownloadR :: Handler RepPlain
getDownloadR = do
    tid   <- lookupGetParam "tid"
    lang  <- lookupGetParams "lang"
    fname <- lookupGetParam "fname"
    lag <- lookupGetParam "lag"
    case (tid, fname, lag) of
         (Just tid', Just fname', Just lag') -> do
             let lagtime = read $ T.unpack lag'
             path <- liftIO $ toSub $ Subtitle tid' lang fname' lagtime SRT
             case path of
                  Just p -> do
                    -- filename "srt/foo.srt" == "foo.srt"
                    let name = FS.filename $ FS.decodeString p
                    addHeader "Content-Disposition" $ 
                        T.pack ("attachment; filename=" ++ FS.encodeString name)
                    sendFile typePlain p
                  _      -> redirect HomeR
         _                  -> redirect HomeR 

getPlayR :: Handler Value
getPlayR = do
    tid   <- lookupGetParam "tid"
    lang  <- lookupGetParams "lang[]"
    fname <- lookupGetParam "fname"
    lag <- lookupGetParam "lag"
    -- req <- getRequest
    pwd <- liftIO getCurrentDirectory
    case (tid, fname, lag) of
         (Just tid', Just fname', Just lag') -> do
             let lagtime = read $ T.unpack lag'
             path <- liftIO $ toSub $ Subtitle tid' lang fname' lagtime VTT
             case path of
                  Just p -> do
                    let vtt = drop (length pwd) p
                    returnJson $ object ["subtitle" .= vtt]
                  _      -> returnJson $ object ["subtitle" .= ("" :: T.Text)]
         _                  -> returnJson $ object ["subtitle" .= ("" :: T.Text)]

getWatchR :: Handler Html
getWatchR = do
    slug <- lookupGetParam "slug"
    sub  <- lookupGetParam "subtitle"
    case (slug, sub) of 
         (Just slug', Just sub') -> do
            let prefix = downloadUrl <> slug'
                audio = prefix <> ".mp3"
                v1500k = prefix <> "-1500k.mp4"
                v950k = prefix <> "-950k.mp4"
                v600k = prefix <> "-600k.mp4"
                v450k = prefix <> "-450k.mp4"
                v320k = prefix <> "-320k.mp4"
                v180k = prefix <> "-180k.mp4"
                v64k = prefix <> "-64k.mp4"
            defaultLayout $ do
                addScript $ StaticR captionator_min_js
                $(widgetFile "watch")
         _             -> redirect HomeR

getTalksR :: Text -> Handler Html
getTalksR url = do
    ((result, widget), enctype) <- runFormGet talkForm
    q' <- case result of
         FormSuccess q -> return q
         _             -> return $ T.concat [talkUrl, url]
    mtalk <- liftIO $ getTalk q'
    case mtalk of
         Just talk -> defaultLayout $ do
             let prefix = downloadUrl <> subName talk 
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
             $(widgetFile "talks")
         _          -> do
             let msg = "ERROR: " <> q' <> " is not a TED talk page!"
             setMessage $ toHtml msg
             redirect HomeR

postSizeR :: Handler Value
postSizeR = do
    url <- lookupPostParam "url"
    size <- lift $ maybe (return 0) responseSize url
    returnJson $ object ["size" .= size]

talkUrl :: Text
talkUrl = "http://www.ted.com/talks/"

downloadUrl :: Text
downloadUrl = "http://download.ted.com/talks/"
