{-# LANGUAGE DeriveGeneric      #-}
module Ted.Types where

import           Data.Aeson
import           Data.Aeson.Types (defaultOptions, Options(..))
import           Data.Text (Text)
import           GHC.Generics (Generic)

-- | Response of https://api.ted.com/v1/talks/:id.json
data QueryResponse = QueryResponse
    { talk          :: Talk
    } deriving (Generic, Show)
instance FromJSON QueryResponse

data Talk = Talk
    { _id           :: Int
    , _name         :: Text
    , _description  :: Text
    , _slug         :: Text
    , _recorded_at  :: Text
    , _published_at :: Text
    , _updated_at   :: Text
    , _viewed_count :: Int
    , _images       :: [Image]
    , _languages    :: Value
    , _tags         :: [Tag]
    , _themes       :: [Theme]
    , _speakers     :: [Speaker]
    } deriving (Generic, Show)
instance FromJSON Talk where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

data Image = Image
    { _image        :: Img
    } deriving (Generic, Show)
instance FromJSON Image where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
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

-- | Response of https://api.ted.com/v1/search.json
data SearchResponse = SearchResponse
    { results       :: [SearchResult]
    } deriving (Generic, Show)
instance FromJSON SearchResponse

data SearchResult = SearchResult
    { _talk       :: SearchTalk
    } deriving (Generic, Show)
instance FromJSON SearchResult where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

data SearchTalk = SearchTalk
    { s_id          :: Int
    , s_name        :: Text
    , s_description :: Text
    , s_slug        :: Text
    , s_recorded_at :: Text
    , s_published_at:: Text
    , s_updated_at  :: Text
    } deriving (Generic, Show)
instance FromJSON SearchTalk where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }

-- | Data used on @{TalksR}
data SubTalk = SubTalk 
    { id            :: Int
    , name          :: Text
    , image         :: Text
    , description   :: Text
    , link          :: Text
    , languages     :: [(Text, Text)]
    , subSlug       :: Text
    , subLag        :: Double
    } deriving Show

