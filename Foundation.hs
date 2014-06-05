{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Foundation where

import           Control.Applicative((<$>), (<*>))
import           Control.Monad (forM, when)
import           Data.Aeson (encode, decodeStrict)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import           Data.Maybe (catMaybes, mapMaybe, fromJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Redis hiding (decode)
import qualified Filesystem.Path.CurrentOS as FS
import           Text.Blaze.Internal (preEscapedText)
import           Text.Jasmine (minifym)
import           Yesod hiding (get)
import           Yesod.Default.Util (addStaticContentExternal)
import           Yesod.Static

import Settings
import Settings.StaticFiles
import Web.TED (getTalkId, queryTalk, searchTalk, SearchTalk(..), toSub, Subtitle(..), FileType(..))
import qualified Web.TED as API
import Handler.Util


data Ted = Ted
    { getStatic :: Static
    , connPool  :: Connection
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
            conn <- fmap connPool getYesod
            emtalks <- lift $ runRedis conn $ do
                elatest <- lrange "latest" 0 4
                mget $ either (const []) id elatest
            let talks = mapMaybe decodeStrict $ catMaybes $ 
                            either (const [Nothing]) id emtalks
            defaultLayout $ do
                setTitle "TED2srt: Download bilingual subtitles of TED talks | Subtitles worth spreading"
                toWidgetHead [hamlet| <meta name=description content="Find out all available subtitle languages, download as plain text or srt file. Watch TED talks with bilingual subtitle. TED演讲双语字幕下载。">|]
                $(widgetFile "homepage")
                $(widgetFile "footer")

getTalksR :: Text -> Handler Html
getTalksR rurl = do
    conn <- fmap connPool getYesod
    emtid <- lift $ runRedis conn $ get $ C8.pack $ T.unpack rurl

    case emtid of
        Right (Just tid) -> do
            result <- lift $ runRedis conn $ multiExec $ do
                t <- get tid
                c <- get ("cache:" <> tid)
                return $ (,) <$> t <*> c
            case result of
                TxSuccess (Just talk, Just cache) ->
                    layout (read $ C8.unpack tid)
                           (fromJust $ decodeStrict talk)
                           (fromJust $ decodeStrict cache)
                TxError err -> setAndRedirect err
                _ -> do
                    talk' <- lift $ queryTalk $ read $ C8.unpack tid
                    case talk' of
                        Just talk -> do
                            let value = apiTalkToValue talk
                            lift $ runRedis conn $
                                setex ("cache:" <> tid) (3600*24)
                                      (L.toStrict $ encode value)
                            getTalksR rurl
                        Nothing   -> notFound
        Right Nothing    -> do
            mtid <- lift $ getTalkId $ talkUrl <> rurl
            case mtid of
                Just tid -> do
                    talk' <- lift $ queryTalk tid
                    case talk' of
                        Nothing   -> notFound
                        Just talk -> do
                            dbtalk <- lift $ marshal talk
                            let value = apiTalkToValue talk
                            lift $ runRedis conn $ multiExec $ do
                                set (C8.pack $ T.unpack $ API.slug talk)
                                    (C8.pack $ show tid)
                                set (C8.pack $ show tid)
                                    (L.toStrict $ encode dbtalk)
                                setex ("cache:" <> C8.pack (show tid))
                                      (3600*24)
                                      (L.toStrict $ encode value)
                            layout tid dbtalk value
                _         -> do
                    let msg = "ERROR: " <> talkUrl <> rurl
                                        <> " is not a TED talk page!"
                    setMessage $ toHtml msg
                    redirect HomeR
        Left reply       -> setAndRedirect reply
  where
    setAndRedirect err = do
        let msg = "ERROR: " <> show err
        setMessage $ toHtml msg
        redirect HomeR
    layout :: Int -> Talk -> TalkCache -> Handler Html
    layout tid dbtalk talk = defaultLayout $ do
        let prefix = downloadUrl <> mSlug dbtalk
            audio = prefix <> ".mp3"
            mu = mediaUrl $ mSlug dbtalk
            v1500k = mu "1500k"
            v950k = mu "950k"
            v600k = mu "600k"
            v320k = prefix <> ".mp4" -- equivalent to "320k"
            clickMsg = "Click to download" :: Text
            rClickMsg = "Right click to download" :: Text
        setTitle $ toHtml $ name dbtalk <> " | Subtitle on ted2srt.org"
        $(widgetFile "topbar")
        $(widgetFile "talks")

-- | Use API function (searchTalk) first.
-- Then query DB, if not in DB, queryTalk and insert.
getSearchR :: Text -> Handler Html
getSearchR q = do
    searchtalks <- lift $ searchTalk q
    when (null searchtalks) notFound
    dbtalks <- forM searchtalks $ \t -> do
        conn <- fmap connPool getYesod
        mtalk <- lift $ runRedis conn $ get (C8.pack $ show $ s_id t)
        case mtalk of
            Right (Just talk') -> return $ decodeStrict talk'
            _                    -> do
                talk' <- lift $ queryTalk $ s_id t
                case talk' of
                    Nothing -> return Nothing
                    Just talk -> do
                        dbtalk <- lift $ marshal talk
                        lift $ runRedis conn $
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
            conn <- fmap connPool getYesod
            path <- lift $ do
                talk <- getTalk conn (read $ T.unpack tid)
                case type_ of
                    "srt" -> toSub $
                        Subtitle tid (slug talk) lang (mSlug talk) (mPad talk) SRT
                    "txt" -> toSub $
                        Subtitle tid (slug talk) lang (mSlug talk) (mPad talk) TXT
                    "lrc" -> toSub $
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

-- Using jwplayer to play video/mp4 with captions.
-- http://www.jwplayer.com
getWatchR :: Handler Html
getWatchR = do
    tid' <- lookupGetParam "tid"
    lang <- lookupGetParams "lang"
    case tid' of
        Just tid -> do
            conn <- fmap connPool getYesod
            talk <- lift $ getTalk conn (read $ T.unpack tid)
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

getTalk :: Connection -> Int -> IO Talk
getTalk conn tid = do
    (Right (Just talk')) <- runRedis conn $
       get (C8.pack $ show tid)
    return $ fromJust $ decodeStrict talk'
