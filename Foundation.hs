{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Foundation where

import           Control.Monad (forM, when)
import           Data.Aeson (encode, decodeStrict)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import           Data.Maybe (catMaybes, fromJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FS
import           Prelude hiding (id)
import           Text.Blaze.Internal (preEscapedText)
import           Text.Jasmine (minifym)
import           Yesod hiding (get)
import           Yesod.Default.Util (addStaticContentExternal)
import           Yesod.Static

import Settings
import Settings.StaticFiles
import Web.TED (getTalkId, queryTalk, searchTalk, SearchTalk(..), toSub, Subtitle(..), FileType(..))
import Handler.Util

import Database.Redis hiding (decode)
import Data.Either.Utils (fromRight)


data Ted = Ted
    { getStatic :: Static
    , conn      :: Connection
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
            | talkUrl `T.isPrefixOf` kw || newTalkUrl `T.isPrefixOf` kw ->
                redirect $ TalksR $ T.drop (T.length talkUrl) kw
            | otherwise -> redirect $ SearchR kw
        _ -> do
            site <- getYesod
            latest <- lift $ runRedis (conn site) $ lrange "latest" 0 4
            talksJson <- lift $ runRedis (conn site) $
                mapM get (fromRight latest)
            let talks = flip map talksJson $ \tj ->
                        fromJust $ decodeStrict $ fromJust $ fromRight tj
            defaultLayout $ do
                setTitle "TED2srt: Download bilingual subtitles of TED talks | Subtitles worth spreading"
                toWidgetHead [hamlet| <meta name=description content="Find out all available subtitle languages, download as plain text or srt file. Watch TED talks with bilingual subtitle. TED演讲双语字幕下载。">|]
                $(widgetFile "homepage")
                $(widgetFile "footer")

getTalksR :: Text -> Handler Html
getTalksR rurl = do
    site <- getYesod
    emtid <- lift $ runRedis (conn site) $ get $ C8.pack $ T.unpack rurl

    case emtid of
        Right (Just tid) -> do
            mtalk <- lift $ runRedis (conn site) $ get tid
            mcache <- lift $ runRedis (conn site) $ get ("cache:" <> tid)
            case (mtalk, mcache) of
                (Right (Just talk), Right (Just cache)) ->
                    layout tid (fromJust $ decodeStrict talk)
                               (fromJust $ decodeStrict cache)
                _ -> do
                    talk' <- liftIO $ queryTalk $ read $ C8.unpack tid
                    case talk' of
                        Just talk -> do
                            let value = apiTalkToValue talk
                            lift $ runRedis (conn site) $
                                setex ("cache:" <> tid) (3600*24)
                                      (L.toStrict $ encode value)
                            -- layout dbtalk value
                            getTalksR rurl
                        Nothing   -> notFound
        _          -> do
            mtid <- liftIO $ getTalkId $ talkUrl <> rurl
            case mtid of
                Just tid -> do
                    talk' <- liftIO $ queryTalk tid
                    case talk' of
                        Nothing   -> notFound
                        Just talk -> do
                            dbtalk <- liftIO $ marshal talk
                            lift $ runRedis (conn site) $ do
                                set (C8.pack $ T.unpack rurl) (C8.pack $ show tid)
                                set (C8.pack $ show tid)
                                    (L.toStrict $ encode dbtalk)
                            getTalksR rurl
                            -- (path, _) <- liftIO $ jsonPath $ talkTid dbtalk
                            -- let value = apiTalkToValue talk
                            -- liftIO $ L.writeFile path $ encode value
                            -- layout dbtalk value
                _         -> do
                    let msg = "ERROR: " <> talkUrl <> rurl
                                        <> " is not a TED talk page!"
                    setMessage $ toHtml msg
                    redirect HomeR
  where
    layout tid' dbtalk talk = defaultLayout $ do
        let prefix = downloadUrl <> mSlug dbtalk
            audio = prefix <> ".mp3"
            mu = mediaUrl $ mSlug dbtalk
            v1500k = mu "1500k"
            v950k = mu "950k"
            v600k = mu "600k"
            v320k = prefix <> ".mp4" -- equivalent to "320k"
            clickMsg = "Click to download" :: Text
            rClickMsg = "Right click to download" :: Text
            tid = C8.unpack tid'
        setTitle $ toHtml $ name dbtalk <> " | Subtitle on ted2srt.org"
        $(widgetFile "topbar")
        $(widgetFile "talks")

-- | Use API function (searchTalk) first.
-- Then query DB, if not in DB, queryTalk and insert.
getSearchR :: Text -> Handler Html
getSearchR q = do
    searchtalks <- liftIO $ searchTalk q
    when (null searchtalks) notFound
    dbtalks <- forM searchtalks $ \t -> do
        site <- getYesod
        mtalk <- lift $ runRedis (conn site) $ get (C8.pack $ show $ s_id t)
        case mtalk of
            Right (Just talk') -> return $ decodeStrict talk'
            _                    -> do
                talk' <- liftIO $ queryTalk $ s_id t
                case talk' of
                    Nothing -> return Nothing
                    Just talk -> do
                        dbtalk <- liftIO $ marshal talk
                        lift $ runRedis (conn site) $
                            set (C8.pack $ show $ s_id t)
                                (L.toStrict $ encode dbtalk)
                        return $ Just dbtalk

    let talks = catMaybes dbtalks
    -- let talks = flip map (catMaybes dbtalks) $ \t ->
    --             t { talkLink = rewriteUrl $ talkLink t }

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
             site <- getYesod
             (Right (Just talk')) <- lift $ runRedis (conn site) $
                get (C8.pack $ T.unpack tid)
             let talk = fromJust $ decodeStrict talk'
             path <- case type_ of
                "srt" -> liftIO $ toSub $
                    Subtitle tid (slug talk) lang (mSlug talk) (mPad talk) SRT
                "txt" -> liftIO $ toSub $
                    Subtitle tid (slug talk) lang (mSlug talk) (mPad talk) TXT
                "lrc" -> liftIO $ toSub $
                    Subtitle tid (slug talk) lang (mSlug talk) (mPad talk) LRC
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
            site <- getYesod
            (Right (Just talk')) <- lift $ runRedis (conn site) $
               get (C8.pack $ T.unpack tid)
            let talk = fromJust $ decodeStrict talk'
            lift $ toSub $
                Subtitle tid (slug talk) lang (mSlug talk) (mPad talk) VTT
            let dataLang = T.intercalate "." lang
            defaultLayout $ do
                addScript $ StaticR jwplayer_jwplayer_js
                $(widgetFile "watch")
        _             -> redirect HomeR

getAboutR :: Handler Html
getAboutR = defaultLayout $ do
    $(widgetFile "topbar")
    $(widgetFile "about")

