module Core.Api
  ( getTalks
  , getTalkTranscript
  , searchTalks
  ) where

import Core.Prelude

import Affjax as AX
import Affjax.ResponseFormat as Res
import Affjax.StatusCode (StatusCode(..))
import Core.Model (Talk)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

type Response a = Aff (Either String a)

handleResponse
  :: forall a
   . DecodeJson a
  => Either AX.Error (AX.Response Json)
  -> Response a
handleResponse = case _ of
  Left err -> do
    let msg = AX.printError err
    void $ liftEffect $ throw msg
    pure $ Left msg
  Right res -> do
    if res.status == StatusCode 200
      then case decodeJson res.body of
        Left msg -> do
          void $ liftEffect $ throw msg
          pure $ Left msg
        Right v -> pure $ Right v
      else case decodeJson res.body of
        Left msg -> do
          void $ liftEffect $ throw msg
          pure $ Left msg
        Right (v :: { message :: String }) -> pure $ Left v.message

get
  :: forall a
   . DecodeJson a
  => AX.URL
  -> Response a
get url = AX.get Res.json url >>= handleResponse

getTalks :: Int -> Response (Array Talk)
getTalks offset = get $ "/api/talks?offset=" <> show offset

getTalkTranscript :: Talk -> String -> Response String
getTalkTranscript talk lang = do
  AX.get Res.string url >>= case _ of
    Left err -> do
      let msg = AX.printError err
      void $ liftEffect $ throw msg
      pure $ Left msg
    Right res -> do
      if res.status == StatusCode 200
        then pure $ Right res.body
        else do
          void $ liftEffect $ throw res.body
          pure $ Left res.body
  where
  url = "/api/talks/" <> show talk.id <> "/transcripts/txt?lang=" <> lang

searchTalks :: String -> Response (Array Talk)
searchTalks q = get $ "/api/search?q=" <> q
