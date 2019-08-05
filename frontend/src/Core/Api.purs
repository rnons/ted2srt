module Core.Api
  ( getTalks
  , getTalkTranscript
  , searchTalks
  ) where

import Prelude

import Affjax (ResponseFormatError(..))
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Control.Apply (lift2)
import Core.Model (Talk)
import Data.Bitraversable (lfor)
import Data.Either (Either(..))
import Data.List.NonEmpty as NonEmpty
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import Foreign (ForeignError(..), MultipleErrors)
import Simple.JSON (class ReadForeign, readJSON)

type Response a = Aff (Either MultipleErrors a)

handleResponse :: forall a. ReadForeign a => AX.Response (Either AX.ResponseFormatError String) -> Response a
handleResponse { status, body } =
  if status == StatusCode 200
    then
      case body of 
        Left (ResponseFormatError err _) -> pure $ Left $ NonEmpty.singleton $ err
        Right response -> lfor (readJSON response) (lift2 (*>) (liftEffect <<< error <<< show) pure)
    else
      pure $ Left $ NonEmpty.singleton $ ForeignError "status code is not 200"

get :: forall a. ReadForeign a => String -> Response a
get url = AX.get ResponseFormat.string url >>= handleResponse

getTalks :: Int -> Response (Array Talk)
getTalks offset = get $ "/api/talks?offset=" <> show offset

getTalkTranscript :: Talk -> String -> Response String
getTalkTranscript talk lang = do
  { status, body } <- AX.get ResponseFormat.string url
  if status == StatusCode 200
    then
      case body of 
        Left (ResponseFormatError err _) -> pure $ Left $ NonEmpty.singleton $ err
        Right response -> pure $ Right response
    else
      pure $ Left $ NonEmpty.singleton $ ForeignError "status code is not 200"
  where
  url = "/api/talks/" <> show talk.id <> "/transcripts/txt?lang=" <> lang

searchTalks :: String -> Response (Array Talk)
searchTalks q = get $ "/api/search?q=" <> q
 

