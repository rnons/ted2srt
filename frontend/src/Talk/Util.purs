module Talk.Util
  ( mkVideoUrl
  , mkTranscriptUrl
  , mkTranscriptDownloadUrl
  ) where

import Core.Prelude

import Core.Model (Talk)
import Talk.Types (SelectedLang(..))

mkVideoUrl :: Talk -> String -> String
mkVideoUrl talk bitrate =
  "https://download.ted.com/talks/"
    <> talk.mediaSlug
    <> "-"
    <> bitrate
    <> ".mp4"

mkTranscriptUrl' :: Boolean -> Talk -> SelectedLang -> String -> String
mkTranscriptUrl' download talk selectedLang format =
  "/api/talks/" <> show talk.id <> "/transcripts/" <> downloadSlug <> format <> query
  where
  query = case selectedLang of
    NoLang -> "?lang=en"
    OneLang lang -> "?lang=" <> lang
    TwoLang lang1 lang2 -> "?lang=" <> lang1 <> "&lang=" <> lang2
  downloadSlug = if download then "download/" else ""

mkTranscriptUrl :: Talk -> SelectedLang -> String -> String
mkTranscriptUrl = mkTranscriptUrl' false

mkTranscriptDownloadUrl :: Talk -> SelectedLang -> String -> String
mkTranscriptDownloadUrl = mkTranscriptUrl' true
