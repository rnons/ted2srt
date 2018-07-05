{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
module Server
  ( tedApi
  , tedServer
  , allApi
  , getBundleH
  ) where

import           Config             (Config (..))
import           Handler.Home       (getHomeH)
import           Handler.Search     (getSearchH)
import           Handler.Subtitles  (downloadSubtitleH, getSubtitleH)
import           Handler.Talks      (getRandomTalkH, getTalkH, getTalksH)
import           Lucid
import           RIO                hiding (Handler)
import           Servant
import           Servant.HTML.Lucid (HTML)
import           Types              (AppM)
import           Types
import           Web.TED            (FileType (..))


instance FromHttpApiData FileType where
  parseUrlPiece "srt" = Right SRT
  parseUrlPiece "vtt" = Right VTT
  parseUrlPiece "txt" = Right TXT
  parseUrlPiece "lrc" = Right LRC
  parseUrlPiece _     = Left "Unsupported"

type TedApi =
       "talks" :> QueryParam "tid" Int          -- ^ getTalksH
               :> QueryParam "limit" Int
               :> Get '[JSON] [Talk]
  :<|> "talks" :> "random"
               :> Get '[JSON] Talk
  :<|> "talks" :> Capture "slug" Text           -- ^ getTalkH
               :> Get '[JSON] Talk
  :<|> "talks" :> Capture "tid" Int             -- ^ getTalkSubtitleH
               :> "transcripts"
               :> Capture "format" FileType
               :> QueryParams "lang" Text
               :> Raw
  :<|> "talks" :> Capture "tid" Int             -- ^ downloadTalkSubtitleH
               :> "transcripts"
               :> "download"
               :> Capture "format" FileType
               :> QueryParams "lang" Text
               :> Raw
  :<|> "search" :> QueryParam "q" Text :> Get '[JSON] [Talk]
  :<|> Get '[HTML] (Html ())

type AllApi = TedApi :<|> "dist" :> Raw

getBundleH :: Server Raw
getBundleH = serveDirectoryWebApp "dist"

tedApi :: Proxy TedApi
tedApi = Proxy

allApi :: Proxy AllApi
allApi = Proxy

tedServer :: Config -> ServerT TedApi AppM
tedServer config =
        getTalksH
  :<|> getRandomTalkH
  :<|> getTalkH
  :<|> (\tid format lang -> Tagged (getSubtitleH config tid format lang))
  :<|> (\tid format lang -> Tagged (downloadSubtitleH config tid format lang))
  :<|> getSearchH
  :<|> getHomeH
