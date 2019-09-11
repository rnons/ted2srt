module Core.Model where

import Core.Prelude

import Data.String as String

type Language =
    { languageCode :: String
    , endonym :: String
    , languageName :: String
    }

type Talk =
    { id :: Int
    , slug :: String
    , image :: String
    , name :: String
    , languages :: Array Language
    , description :: String
    , mediaSlug :: String
    -- , publishedAt :: Maybe Date.Date
    }

getTitleSpeaker :: Talk -> { title :: String, speaker :: String }
getTitleSpeaker { name } =
  { title:
      String.drop 2 $ String.dropWhile (_ /= String.codePointFromChar ':') name
  , speaker: String.takeWhile (_ /= String.codePointFromChar ':') name
  }

unescape :: String -> String
unescape =
  String.replaceAll (String.Pattern "&lt;") (String.Replacement "<") >>>
  String.replaceAll (String.Pattern "&gt;") (String.Replacement ">") >>>
  String.replaceAll (String.Pattern "&#39;") (String.Replacement "\'") >>>
  String.replaceAll (String.Pattern "&quot;") (String.Replacement "\"") >>>
  String.replaceAll (String.Pattern "&amp;") (String.Replacement "&")
