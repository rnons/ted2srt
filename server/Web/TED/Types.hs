{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards  #-}
module Web.TED.Types
  ( Talk (..)
  , SearchTalk (..)
  , Image (..)
  , Language (..)
  , Tag (..)
  , Theme (..)
  , TM (..)
  , Speaker (..)
  , Sp (..)
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (mzero, liftM)
import           Data.Aeson
import           Data.Aeson.Types (defaultOptions, Options(..), parseMaybe)
import qualified Data.HashMap.Strict as HM
import           Data.List (sort)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.Time.Format (parseTimeM, defaultTimeLocale)
import qualified Data.Vector as V
import           GHC.Generics (Generic)


newtype TEDTime = TEDTime { fromTEDTime :: UTCTime }

instance FromJSON TEDTime where
    parseJSON = withText "TEDTime" $ \t ->
        case parseTimeM True defaultTimeLocale timeFormat (T.unpack t) of
            Just time -> return $ TEDTime time
            Nothing   -> fail $ "Failed to parse TED time: " ++ T.unpack t
      where
        timeFormat = "%Y-%m-%d %H:%M:%S"

data Image = Image
    { small :: Text
    , medium :: Text
    } deriving (Generic, Show)
instance FromJSON Image
instance ToJSON Image

newtype TEDImage = TEDImage { fromTEDImage :: Image }

instance FromJSON TEDImage where
    parseJSON (Array v) =
        let urls = V.map parseImage v
        in  TEDImage <$> (Image <$> (urls V.! 1) <*> (urls V.! 2))
      where
        parseImage (Object o) =
            o .: "image" >>= (.: "url")
        parseImage _ = mzero
    parseJSON _ = mzero

data Language = Language
    { languageName :: Text
    , languageCode :: Text
    } deriving (Show)
instance FromJSON Language where
    parseJSON (Object v) =
        Language <$> v .: "name"
                 <*> v .: "code"
    parseJSON _          = mzero
instance ToJSON Language where
    toJSON Language{..} =
        object [ "name" .= languageName
               , "code" .= languageCode
               ]
newtype Languages = Languages { fromLanguages :: [Language] }

instance FromJSON Languages where
    parseJSON (Object v) =
        let langCode = HM.keys v
            langName = flip map (HM.elems v) $ \lang ->
                case parseMaybe parseLanguage lang of
                    Just name -> name
                    Nothing   -> "Failed"
        in  return $ Languages $ map (uncurry Language) $ sort $ zip langName langCode
      where
        parseLanguage = withObject "language" $ \lang ->
            withText "name" return $ lang HM.! "name"
    parseJSON _ = mzero

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

-- | Some talks (performance) has no language infomation e.g. 581.
data Talk = Talk
    { id           :: Int
    , name         :: Text
    , description  :: Text
    , slug         :: Text
    , recordedAt   :: UTCTime
    , publishedAt  :: UTCTime
    , updatedAt    :: UTCTime
    , viewedCount  :: Int
    , images       :: Image
    , media        :: Value
    , languages    :: [Language]
    , tags         :: [Tag]
    , themes       :: [Theme]
    , speakers     :: [Speaker]
    } deriving (Generic, Show)
instance FromJSON Talk where
    parseJSON (Object v) =
        Talk <$> v .: "id"
             <*> v .: "name"
             <*> v .: "description"
             <*> v .: "slug"
             <*> liftM fromTEDTime (v .: "recorded_at")
             <*> liftM fromTEDTime (v .: "published_at")
             <*> liftM fromTEDTime (v .: "updated_at")
             <*> v .: "viewed_count"
             <*> liftM fromTEDImage (v .: "images")
             <*> v .: "media"
             <*> (v .: "languages" >>= return . fromLanguages)
             <*> v .: "tags"
             <*> v .: "themes"
             <*> v .: "speakers"
    parseJSON _          = mzero

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

