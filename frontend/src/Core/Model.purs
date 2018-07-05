module Core.Model where

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
    -- , title :: String
    -- , speaker :: String
    , languages :: Array Language
    , description :: String
    , mediaSlug :: String
    -- , publishedAt :: Maybe Date.Date
    }
