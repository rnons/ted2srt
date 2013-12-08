{-# LANGUAGE DeriveGeneric      #-}
module Ted.Types where

import           Data.Aeson
import           Data.Aeson.Types (defaultOptions, Options(..))
import           Data.Text (Text)
import           GHC.Generics (Generic)

data QueryTalk = QueryTalk
    { talk          :: Talk
    } deriving (Generic, Show)
instance FromJSON QueryTalk

data Talk = Talk
    { id            :: Int
    , name          :: Text
    , description   :: Text
    , slug          :: Text
    , recorded_at   :: Text
    , published_at  :: Text
    , updated_at    :: Text
    , viewed_count  :: Int
    , images        :: [Image]
    , languages     :: Value
    , tags          :: [Tag]
    , themes        :: [Theme]
    , speakers      :: [Speaker]
    } deriving (Generic, Show)
instance FromJSON Talk

data Image = Image
    { image         :: Img
    } deriving (Generic, Show)
instance FromJSON Image
data Img = Img
    { size          :: Text
    , url           :: Text
    } deriving (Generic, Show)
instance FromJSON Img
    
data Tag = Tag
    { tag           :: Text
    } deriving (Generic, Show)
instance FromJSON Tag

data Theme = Theme
    { theme         :: TM
    } deriving (Generic, Show)
instance FromJSON Theme
data TM = TM
    { tm_id         :: Int
    , tm_name       :: Text
    } deriving (Generic, Show)
instance FromJSON TM where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 3 }

data Speaker = Speaker
    { speaker       :: Sp
    } deriving (Generic, Show)
instance FromJSON Speaker
data Sp = Sp
    { sp_id         :: Int
    , sp_name       :: Text
    } deriving (Generic, Show)
instance FromJSON Sp where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 3 }

data SubTalk = SubTalk 
    { tid               :: Int
    , title             :: Text
    , intro             :: Text
    , link              :: Text
    , subLang           :: [(Text, Text)]
    , subName           :: Text
    , subLag            :: Double
    } deriving Show

