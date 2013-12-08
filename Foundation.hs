{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Foundation where

import qualified Control.Exception.Lifted as E
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Persist.Postgresql (PostgresConf)
import           Database.Persist.Sql (SqlPersistT)
import qualified Filesystem.Path.CurrentOS as FS
import           Prelude hiding (id)
import           System.Directory
import           Text.Blaze.Internal (Markup)
import           Text.Jasmine (minifym)
import           Text.Julius (rawJS)
import           Yesod hiding (languages)
import           Yesod.Default.Util (addStaticContentExternal)
import           Yesod.Static

import Model
import Settings
import Ted
import Ted.Types (Talk(..), SubTalk(..))
import Ted.Fallback (getTalk)


data Ted = Ted
    { getStatic :: Static
    , connPool :: PersistConfigPool PostgresConf
    , persistConfig :: PostgresConf
    }

mkYesod "Ted" [parseRoutes|
/static StaticR Static getStatic
/favicon.ico FaviconR GET
/ HomeR GET
/download DownloadR GET
/play PlayR GET
/watch WatchR GET
/talks/#Text TalksR GET
|]

instance Yesod Ted where
    addStaticContent = 
        addStaticContentExternal minifym genFileName staticDir (StaticR . flip StaticRoute [])
      where
        genFileName = base64md5

    -- Override defaultLayout to apply default.cassius to all pages.
    defaultLayout widget = do
        pc <- widgetToPageContent $ do 
            widget
            $(widgetFile "default")
        giveUrlRenderer
            [hamlet|
                $doctype 5
                <html>
                    <head>
                        <title>#{pageTitle pc}
                        <meta charset=utf-8>
                        ^{pageHead pc}
                    <body>
                        <article>
                            ^{pageBody pc}
            |]

instance YesodPersist Ted where
    type YesodPersistBackend Ted = SqlPersistT
    runDB = defaultRunDB persistConfig connPool

instance RenderMessage Ted FormMessage where
    renderMessage _ _ = defaultFormMessage

talkForm :: Markup -> MForm (HandlerT Ted IO) (FormResult Text, Widget)
talkForm = renderDivs $ areq talkUrlField ""
                                         { fsId = Just "search_input"
                                         , fsName = Just "q"
                                         , fsAttrs = [ ("placeholder", "URL")
                                                     , ("autofocus", "true")
                                                     ]
                                         } Nothing
  where
    errorMessage :: Text
    errorMessage = "Valid url is like this http://www.ted.com/talks/thomas_heatherwick.html"
    
    talkUrlField = check validUrl textField
    validUrl x
        | talkUrl `T.isPrefixOf` x = Right x
        | otherwise                = Left errorMessage

getFaviconR :: MonadHandler m => m ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

-- Homepage consists talkForm and talkTile
-- Input valid TED talk page url into talkForm, all avialable subtitles
-- will be returned.
-- talkTile displays five recent talks. There is a cron job running per 12
-- hours to fetch the recent talks. (fetch.hs)
getHomeR :: Handler Html
getHomeR = do
    ((result, widget), enctype) <- runFormGet talkForm
    case result of
        FormSuccess q -> redirect $ TalksR $ T.drop (T.length talkUrl) q
        _       -> do
            talks' <- E.catch (runDB $ selectList [] [Desc TalkTid, LimitTo 5])
                              (\e -> liftIO $ print (e :: E.SomeException) >> 
                               return [])
            let talks = flip map talks' $ \(Entity _ t) ->
                    t { talkLink = rewriteUrl $ talkLink t }
            defaultLayout $ do
                setTitle "Ted2srt: Download bilingual subtitles of TED talks | Subtitles worth spreading"
                toWidgetHead [hamlet| <meta name=description content="Find out all available subtitle languages, download as plain text or srt file. Watch TED talks with bilingual subtitle. TED演讲双语字幕下载。">|]
                $(widgetFile "homepage")
  where
    -- substitute ted.com to ted2srt.org  e.g.
    -- http://www.ted.com/talks/marla_spivak_why_bees_are_disappearing.html
    rewriteUrl url = "http://ted2srt.org" <> T.drop 18 url

getDownloadR :: Handler RepPlain
getDownloadR = do
    tid   <- lookupGetParam "tid"
    lang  <- lookupGetParams "lang"
    fname <- lookupGetParam "fname"
    lag <- lookupGetParam "lag"
    type_   <- lookupGetParam "type"
    case (tid, fname, lag, type_) of
         (Just tid', Just fname', Just lag', Just type') -> do
             let lagtime = read $ T.unpack lag'
             path <- case type' of
                "srt" -> liftIO $ toSub $ Subtitle tid' lang fname' lagtime SRT
                "txt" -> liftIO $ toSub $ Subtitle tid' lang fname' lagtime TXT
                _     -> return Nothing
             case path of
                  Just p -> do
                    -- filename "srt/foo.srt" == "foo.srt"
                    let fn = FS.filename $ FS.decodeString p
                    addHeader "Content-Disposition" $ 
                        T.pack ("attachment; filename=" ++ FS.encodeString fn)
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
                  _      -> returnJson $ object ["subtitle" .= ("" :: Text)]
         _  -> returnJson $ object ["subtitle" .= ("" :: Text)]

-- Using video.js to play video/mp4 with captions.
-- https://github.com/videojs/video.js/
getWatchR :: Handler Html
getWatchR = do
    slug <- lookupGetParam "slug"
    sub  <- lookupGetParam "subtitle"
    case (slug, sub) of 
         (Just slug', Just sub') -> do
            let mu = mediaUrl slug'
                v950k = mu "950k"
                v600k = mu "600k"
                v450k = mu "450k"
            defaultLayout $ do
                addScriptRemote "http://jwpsrv.com/library/_trNSDy3EeO49hIxOQfUww.js"
                $(widgetFile "watch")
         _             -> redirect HomeR

getTalksR :: Text -> Handler Html
getTalksR url = do
    ((result, widget), _) <- runFormGet talkForm
    q' <- case result of
         FormSuccess q -> return q
         _             -> return $ talkUrl <> url
    mtalk <- runDB $ selectFirst [TalkLink ==. url] []
    case mtalk of
        Just (Entity _ talk') -> do
            tedtalk <- liftIO $ queryTalk $ talkTid talk'
            let stalk = SubTalk { id = _id tedtalk
                                , name = _name tedtalk
                                , image = talkImg tedtalk
                                , description = _description tedtalk
                                , link = talkUrl <> _slug tedtalk <> ".html"
                                , languages = talkLanguages tedtalk
                                , subSlug = talkMediaSlug talk'
                                , subLag = talkMediaPad talk'
                                }
            layout stalk widget    
        _          -> do
            mtalk' <- liftIO $ getTalk q'
            case mtalk' of
                Just talk -> do
                    let dbtalk = Model.Talk { talkTid = id talk
                                            , talkTitle = name talk
                                            , talkLink = link talk
                                            , talkImage = image talk
                                            , talkMediaSlug = subSlug talk
                                            , talkMediaPad = subLag talk
                                            }
                    runDB $ insertUnique dbtalk
                    layout talk widget
                _         -> do
                    let msg = "ERROR: " <> q' <> " is not a TED talk page!"
                    setMessage $ toHtml msg
                    redirect HomeR
  where
    layout talk widget = defaultLayout $ do
        let prefix = downloadUrl <> subSlug talk 
            audio = prefix <> ".mp3"
            mu = mediaUrl $ subSlug talk
            v1500k = mu "1500k"
            v950k = mu "950k"
            v600k = mu "600k"
            v320k = mu "320k"
        setTitle $ toHtml $ name talk <> " | Subtitle on ted2srt.org"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
        $(widgetFile "talks")

talkUrl :: Text
talkUrl = "http://www.ted.com/talks/"

downloadUrl :: Text
downloadUrl = "http://download.ted.com/talks/"

-- Available quality: 1500k, 950k, 600k, 450k, 320k, 180k, 64k
mediaUrl :: Text -> Text -> Text
mediaUrl part quality =
  downloadUrl <> part <> "-" <> quality <> ".mp4"
