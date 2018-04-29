{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
module Server
  ( tedApi
  , tedServer
  ) where

import           RIO               hiding (Handler)
import           Servant
import           Types             (AppM)

import           Config            (Config (..))
import           Handler.Search    (getSearchH)
import           Handler.Subtitles (downloadSubtitleH, getSubtitleH)
import           Handler.Talks     (getRandomTalkH, getTalkH, getTalksH)
import           Types
import           Web.TED           (FileType (..))


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

tedApi :: Proxy TedApi
tedApi = Proxy

tedServer :: Config -> ServerT TedApi AppM
tedServer config =
        getTalksH
  :<|> getRandomTalkH
  :<|> getTalkH
  :<|> (\tid format lang -> Tagged (getSubtitleH config tid format lang))
  :<|> (\tid format lang -> Tagged (downloadSubtitleH config tid format lang))
  :<|> getSearchH
