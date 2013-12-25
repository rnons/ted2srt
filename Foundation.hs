{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module Foundation where

import qualified Control.Exception.Lifted as E
import           Control.Monad (forM)
import qualified Data.ByteString.Char8 as B8
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Persist.Postgresql (PostgresConf)
import           Database.Persist.Sql (SqlPersistT)
import qualified Filesystem.Path.CurrentOS as FS
import           Prelude hiding (id)
import           System.Directory
import           Text.Blaze.Internal (preEscapedText)
import           Text.Jasmine (minifym)
import           Text.Julius (rawJS)
import           Yesod hiding (languages)
import           Yesod.Default.Util (addStaticContentExternal)
import           Yesod.Static

import Model
import Settings
import Ted
import Ted.Types (Talk(..), SubTalk(..), SearchTalk(..))
import Ted.Fallback (getTalk, getSlugAndPad)


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
            talks' <- E.catch (runDB $ selectList [] [Desc TalkTid, LimitTo 5])
                              (\e -> liftIO $ print (e :: E.SomeException) >> 
                               return [])
            let talks = flip map talks' $ \(Entity _ t) ->
                        t { talkLink = rewriteUrl $ talkLink t }
            defaultLayout $ do
                setTitle "Ted2srt: Download bilingual subtitles of TED talks | Subtitles worth spreading"
                toWidgetHead [hamlet| <meta name=description content="Find out all available subtitle languages, download as plain text or srt file. Watch TED talks with bilingual subtitle. TED演讲双语字幕下载。">|]
                $(widgetFile "homepage")

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

getSearchR :: Text -> Handler Html
getSearchR q = do
    searchtalks <- liftIO $ searchTalk $ B8.pack $ T.unpack q
    dbtalks <- forM searchtalks $ \t -> do
        mtalk <- runDB $ selectFirst [TalkTid ==. s_id t] []
        case mtalk of 
            Just (Entity _ talk') -> return talk'
            _                    -> do
                tedtalk <- liftIO $ queryTalk $ s_id t
                (slug, pad) <- liftIO $ getSlugAndPad $ tedTalkUrl $ s_slug t
                let dbtalk = Model.Talk { talkTid = _id tedtalk
                                        , talkTitle = _name tedtalk
                                        , talkDescription = s_description t
                                        , talkLink = tedTalkUrl $ s_slug t
                                        , talkImage = talkImg tedtalk
                                        , talkMediaSlug = slug
                                        , talkMediaPad = pad
                                        }
                runDB $ insertUnique dbtalk
                return dbtalk
                                        
    let talks = flip map dbtalks $ \t ->
                t { talkLink = rewriteUrl $ talkLink t }
            
    defaultLayout $ do
        $(widgetFile "topbar")
        $(widgetFile "search")

getTalksR :: Text -> Handler Html
getTalksR url = do
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
            layout stalk
        _          -> do
            mtalk' <- liftIO $ getTalk $ tedTalkUrl url
            case mtalk' of
                Just talk -> do
                    let dbtalk = Model.Talk { talkTid = id talk
                                            , talkTitle = name talk
                                            , talkDescription = description talk
                                            , talkLink = link talk
                                            , talkImage = image talk
                                            , talkMediaSlug = subSlug talk
                                            , talkMediaPad = subLag talk
                                            }
                    runDB $ insertUnique dbtalk
                    layout talk
                _         -> do
                    let msg = "ERROR: " <> url <> " is not a TED talk page!"
                    setMessage $ toHtml msg
                    redirect HomeR
  where
    layout talk = defaultLayout $ do
        let prefix = downloadUrl <> subSlug talk 
            audio = prefix <> ".mp3"
            mu = mediaUrl $ subSlug talk
            v1500k = mu "1500k"
            v950k = mu "950k"
            v600k = mu "600k"
            v320k = mu "320k"
        setTitle $ toHtml $ name talk <> " | Subtitle on ted2srt.org"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
        $(widgetFile "topbar")
        $(widgetFile "talks")

talkUrl :: Text
talkUrl = "http://www.ted.com/talks/"

downloadUrl :: Text
downloadUrl = "http://download.ted.com/talks/"

-- Available quality: 1500k, 950k, 600k, 450k, 320k, 180k, 64k
mediaUrl :: Text -> Text -> Text
mediaUrl part quality =
  downloadUrl <> part <> "-" <> quality <> ".mp4"

myTalkUrl :: Text -> Text
myTalkUrl talkslug = "http://ted2srt.org/talks/" <> talkslug <> ".html"

tedTalkUrl :: Text -> Text
tedTalkUrl talkslug = "http://www.ted.com/talks/" <> talkslug <> ".html"

img113x85 :: Text -> Text
img113x85 url = (T.reverse. T.drop 11 . T.reverse) url <> "113x85.jpg"

-- Drop the talkUrl part.
-- e.g. marla_spivak_why_bees_are_disappearing.html
rewriteUrl ::Text -> Text
rewriteUrl = T.drop $ T.length talkUrl
