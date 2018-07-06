module Core.Api
  ( getTalks
  , getTalkTranscript
  ) where

import Prelude

import Control.Apply (lift2)
import Core.Model (Talk)
import Data.Bitraversable (lfor)
import Data.Either (Either(..))
import Data.List.NonEmpty as NonEmpty
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Foreign (ForeignError(..), MultipleErrors)
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as Response
import Network.HTTP.StatusCode (StatusCode(..))
import Simple.JSON (class ReadForeign, readJSON)

type Response a = Aff (Either MultipleErrors a)

handleResponse :: forall a. ReadForeign a => AffjaxResponse String -> Response a
handleResponse { status, response } =
  if status == StatusCode 200
    then
      lfor (readJSON response) (lift2 (*>) (liftEffect <<< error <<< show) pure)
    else
      pure $ Left $ NonEmpty.singleton $ ForeignError "status code is not 200"

get :: forall a. ReadForeign a => String -> Response a
get url = AX.get Response.string url >>= handleResponse

getTalks :: Response (Array Talk)
getTalks = get "/api/talks?limit=5"

getTalkTranscript :: Talk -> Response String
getTalkTranscript talk = do
  -- get $ "/api/talks/" <> show talk.id <> "/transcripts/txt?lang=en"
  { status, response } <- AX.get Response.string url
  if status == StatusCode 200
    then pure $ Right response
    else
      pure $ Left $ NonEmpty.singleton $ ForeignError "status code is not 200"
  where
  url = "/api/talks/" <> show talk.id <> "/transcripts/txt?lang=en"
