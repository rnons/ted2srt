{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Foundation where

import qualified Control.Exception.Lifted as E
import           Control.Monad (forM)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Persist.Postgresql (PostgresConf)
import           Database.Persist.Sql (SqlPersistT)
import qualified Filesystem.Path.CurrentOS as FS
import           Prelude hiding (id)
import           Text.Blaze.Internal (preEscapedText)
import           Text.Jasmine (minifym)
import           Yesod
import           Yesod.Default.Util (addStaticContentExternal)
import           Yesod.Static

import Model
import Settings
import Settings.StaticFiles
import Web.TED
import Handler.Util


data Ted = Ted
    { getStatic :: Static
    , connPool :: PersistConfigPool PostgresConf
    , persistConfig :: PostgresConf
    }

mkYesod "Ted" [parseRoutes|
/static StaticR Static getStatic
/favicon.ico FaviconR GET
/ HomeR GET
/about AboutR GET
/download DownloadR GET
/watch WatchR GET
/talks/#Text TalksR GET
/search/#Text SearchR GET
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
        mmsg <- getMessage
        giveUrlRenderer
            [hamlet|
                $doctype 5
                <html>
                    <head>
                        <title>#{pageTitle pc}
                        <meta charset=utf-8>
                        <meta name=viewport content="width=device-width, initial-scale=1">
                        ^{pageHead pc}
                    <body>
                        $maybe msg <- mmsg
                            <div .message>#{msg}
                        ^{pageBody pc}
            |]

    errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
        setTitle "TED2srt | Page Not Found."
        $(widgetFile "topbar")
        $(widgetFile "404")
        $(widgetFile "footer")
    errorHandler other = defaultErrorHandler other

instance YesodPersist Ted where
    type YesodPersistBackend Ted = SqlPersistT
    runDB = defaultRunDB persistConfig connPool

instance RenderMessage Ted FormMessage where
    renderMessage _ _ = defaultFormMessage

getFaviconR :: MonadHandler m => m ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

-- Homepage consists talkForm and talkTile
-- talkForm accepts talk url or keywords, redirct to TalkR or SearchR
-- talkTile displays five recent talks. There is a cron job running per 12
-- hours to fetch the recent talks. (fetch.hs)
getHomeR :: Handler Html
getHomeR = do
    q <- lookupGetParam "q"
    case q of
        Just kw
            | talkUrl `T.isPrefixOf` kw ->
                redirect $ TalksR $ T.drop (T.length talkUrl) kw
            | otherwise -> redirect $ SearchR kw
        _ -> do
            talks' <- E.catch (runDB $ selectList [] [Desc TalkPublishedAt, LimitTo 5])
                              (\e -> liftIO $ print (e :: E.SomeException) >>
                               return [])
            let talks = flip map talks' $ \(Entity _ t) ->
                        t { talkLink = rewriteUrl $ talkLink t }
            defaultLayout $ do
                setTitle "TED2srt: Download bilingual subtitles of TED talks | Subtitles worth spreading"
                toWidgetHead [hamlet| <meta name=description content="Find out all available subtitle languages, download as plain text or srt file. Watch TED talks with bilingual subtitle. TED演讲双语字幕下载。">|]
                $(widgetFile "homepage")
                $(widgetFile "footer")

getTalksR :: Text -> Handler Html
getTalksR rurl = do
    mtalk <- runDB $ selectFirst [TalkLink ==. rurl] []
    case mtalk of
        Just (Entity _ dbtalk) -> do
            talk <- liftIO $ queryTalk $ talkTid dbtalk
            layout dbtalk talk
        _          -> do
            mtid <- liftIO $ getTalkId $ talkUrl <> rurl
            case mtid of
                Just tid -> do
                    talk <- liftIO $ queryTalk tid
                    dbtalk <- liftIO $ marshal talk
                    runDB $ insertUnique dbtalk
                    layout dbtalk talk
                _         -> do
                    let msg = "ERROR: " <> talkUrl <> rurl
                                        <> " is not a TED talk page!"
                    setMessage $ toHtml msg
                    redirect HomeR
  where
    layout dbtalk talk = defaultLayout $ do
        let prefix = downloadUrl <> talkMediaSlug dbtalk
            audio = prefix <> ".mp3"
            mu = mediaUrl $ talkMediaSlug dbtalk
            v1500k = mu "1500k"
            v950k = mu "950k"
            v600k = mu "600k"
            v320k = mu "320k"
        setTitle $ toHtml $ name talk <> " | Subtitle on ted2srt.org"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
        $(widgetFile "topbar")
        $(widgetFile "talks")

-- | Use API function (searchTalk) first.
-- Then query DB, if not in DB, queryTalk and insert.
getSearchR :: Text -> Handler Html
getSearchR q = do
    searchtalks <- liftIO $ searchTalk q
    dbtalks <- forM searchtalks $ \t -> do
        mtalk <- runDB $ getBy (UniqueTalk $ s_id t)
        case mtalk of
            Just (Entity _ talk') -> return talk'
            _                    -> do
                dbtalk <- liftIO $ marshal =<< queryTalk (s_id t)
                runDB $ insertUnique dbtalk
                return dbtalk

    let talks = flip map dbtalks $ \t ->
                t { talkLink = rewriteUrl $ talkLink t }

    defaultLayout $ do
        $(widgetFile "topbar")
        $(widgetFile "search")

getDownloadR :: Handler RepPlain
getDownloadR = do
    tid'  <- lookupGetParam "tid"
    lang  <- lookupGetParams "lang"
    type' <- lookupGetParam "type"
    case (tid', type') of
         (Just tid, Just type_) -> do
             (Entity _ talk) <- runDB $ getBy404 (UniqueTalk $ read $ T.unpack tid)
             path <- case type_ of
                "srt" -> liftIO $ toSub $
                    Subtitle tid lang (talkMediaSlug talk) (talkMediaPad talk) SRT
                "txt" -> liftIO $ toSub $
                    Subtitle tid lang (talkMediaSlug talk) (talkMediaPad talk) TXT
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

-- Using video.js to play video/mp4 with captions.
-- https://github.com/videojs/video.js/
getWatchR :: Handler Html
getWatchR = do
    tid' <- lookupGetParam "tid"
    lang <- lookupGetParams "lang"
    case tid' of
        Just tid -> do
            (Entity _ talk) <- runDB $ getBy404 (UniqueTalk $ read $ T.unpack tid)
            path' <- liftIO $ toSub $
                Subtitle tid lang (talkMediaSlug talk) (talkMediaPad talk) VTT
            case path' of
                Just path -> do
                    let dataLang = T.intercalate "." lang
                    defaultLayout $ do
                        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
                        addScript $ StaticR jwplayer_jwplayer_js
                        $(widgetFile "watch")
                _ -> redirect HomeR
        _             -> redirect HomeR

getAboutR :: Handler Html
getAboutR = defaultLayout $ do
    $(widgetFile "topbar")
    $(widgetFile "about")

