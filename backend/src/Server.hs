{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
module Server
  ( tedApiView
  , tedServer
  , allApi
  , getBundleH
  ) where

import           Api.Search         (getSearchApiH)
import           Api.Subtitles      (downloadSubtitleH, getSubtitleH)
import           Api.Talks          (getRandomTalkApiH, getTalkApiH,
                                     getTalksApiH)
import           Config             (Config (..))
import           Database.Persist   (Entity (..))
import           Lucid
import           RIO                hiding (Handler)
import           Servant
import           Servant.HTML.Lucid (HTML)
import           Types              (AppM)
import           Types
import           View.Home          (getHomeH)
import           View.Search        (getSearchH)
import           View.Talk          (getTalkH)
import           Web.TED            (FileType (..))


instance FromHttpApiData FileType where
  parseUrlPiece "srt" = Right SRT
  parseUrlPiece "vtt" = Right VTT
  parseUrlPiece "txt" = Right TXT
  parseUrlPiece "lrc" = Right LRC
  parseUrlPiece _     = Left "Unsupported"

type TedApi =
       "talks"
    :> QueryParam "offset" Int          -- ^ getTalksH
    :> QueryParam "limit" Int
    :> Get '[JSON] [Entity Talk]
  :<|> "talks" :> "random"
    :> Get '[JSON] Talk
  :<|> "talks"
    :> Capture "slug" Text           -- ^ getTalkH
    :> Get '[JSON] Talk
  :<|> "talks"
    :> Capture "tid" Int             -- ^ getTalkSubtitleH
    :> "transcripts"
    :> Capture "format" FileType
    :> QueryParams "lang" Text
    :> Raw
  :<|> "talks"
    :> Capture "tid" Int             -- ^ downloadTalkSubtitleH
    :> "transcripts"
    :> "download"
    :> Capture "format" FileType
    :> QueryParams "lang" Text
    :> Raw
  :<|> "search"
    :> QueryParam "q" Text :> Get '[JSON] [Entity Talk]

type TedView =
       Get '[HTML] (Html ())
  :<|> "talks" :> Capture "slug" Text :> Get '[HTML] (Html ())
  :<|> "search" :> QueryParam "q" Text :> Get '[HTML] (Html ())

type TedApiView =
      "api" :> TedApi
  :<|> TedView

type AllApi =
       TedApiView
  :<|> "dist" :> Raw

getBundleH :: Server Raw
getBundleH = serveDirectoryWebApp "dist"

tedApiView :: Proxy TedApiView
tedApiView = Proxy

allApi :: Proxy AllApi
allApi = Proxy

tedApiServer :: Config -> ServerT TedApi AppM
tedApiServer config =
       getTalksApiH
  :<|> getRandomTalkApiH
  :<|> getTalkApiH
  :<|> (\tid format lang -> Tagged (getSubtitleH config tid format lang))
  :<|> (\tid format lang -> Tagged (downloadSubtitleH config tid format lang))
  :<|> getSearchApiH


tedViewServer :: ServerT TedView AppM
tedViewServer =
       getHomeH
  :<|> getTalkH
  :<|> getSearchH

tedServer :: Config -> ServerT TedApiView AppM
tedServer config =
       tedApiServer config
  :<|> tedViewServer
