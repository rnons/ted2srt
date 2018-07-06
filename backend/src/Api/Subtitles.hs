module Api.Subtitles
  ( getSubtitleH
  , downloadSubtitleH
  ) where

import qualified Data.ByteString.Char8     as C
import qualified Filesystem.Path.CurrentOS as FS
import           Models.Talk               (getTalkById)
import           Network.HTTP.Types        (status200, status404)
import           Network.Wai               (Application, Response, responseFile,
                                            responseLBS)
import           RIO
import           Types
import           Web.TED                   (FileType (..), Subtitle (..), toSub)


getSubtitlePath :: Int -> FileType -> [Text] -> RIO Config (Maybe FilePath)
getSubtitlePath tid format lang = do
  mTalk <- getTalkById tid Nothing
  case mTalk of
    Just (Talk {..}) -> liftIO $ toSub $
      Subtitle tid _talkSlug lang _talkMediaSlug _talkMediaPad format
    Nothing -> return Nothing

notFound :: (Response -> t) -> t
notFound respond = respond $ responseLBS status404 [] "Not Found"

getSubtitleH :: Config -> Int -> FileType -> [Text] -> Application
getSubtitleH config tid format lang _ respond = do
  let cType = if format == VTT then "text/vtt" else "text/plain"
  path <- runRIO config $ getSubtitlePath tid format lang
  case path of
    Just p  -> respond $ responseFile status200 [("Content-Type", cType)] p Nothing
    Nothing -> notFound respond

downloadSubtitleH :: Config -> Int -> FileType -> [Text] -> Application
downloadSubtitleH config tid format lang _ respond = do
  -- path <- lift $ runRIO config $ getSubtitlePath tid format lang
  path <- runRIO config $ getSubtitlePath tid format lang
  case path of
    Just p  -> do
      let filename = C.pack $ FS.encodeString $ FS.filename $ FS.decodeString p
      respond $ responseFile
        status200
        [ ("Content-Type", "text/plain")
        , ("Content-Disposition", "attachment; filename=" <> filename)]
        p
        Nothing
    Nothing -> notFound respond
