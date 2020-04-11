{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

module Model where

import           Data.Aeson                       (FromJSON, ToJSON, Value)
import           Database.Persist.Postgresql.JSON ()
import           Database.Persist.TH
import           RIO
import           RIO.Time                         (UTCTime)

data Language = Language
  { languageName :: Text
  , languageCode :: Text
  , endonym      :: Text
  } deriving (Generic)
instance FromJSON Language
instance ToJSON Language

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Talk json
    Id Int sql=id
    name Text
    slug Text
    filmedAt UTCTime
    publishedAt UTCTime
    description Text
    image Text
    languages Value
    mediaSlug Text
    mediaPad Double
|]
