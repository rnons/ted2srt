{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Model where

import           Data.Aeson
import qualified Data.Char                            as Char
import           Data.Time                            (UTCTime)
import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92      (HasSqlValueSyntax)
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax        (PgValueSyntax)
import qualified Database.PostgreSQL.Simple           as DB
import qualified Database.PostgreSQL.Simple.FromField as DB
import           RIO

data Language = Language
  { languageName :: Text
  , languageCode :: Text
  , endonym      :: Text
  } deriving (Generic, Eq, Show)
instance FromJSON Language
instance ToJSON Language

instance DB.FromField [Language] where
    fromField = DB.fromJSONField

instance FromBackendRow Postgres [Language]
instance HasSqlValueSyntax PgValueSyntax [Language]

data TalkT f = Talk
  { _talkId          :: Columnar f Int
  , _talkName        :: Columnar f Text
  , _talkSlug        :: Columnar f Text
  , _talkFilmed      :: Columnar f UTCTime
  , _talkPublished   :: Columnar f UTCTime
  , _talkDescription :: Columnar f Text
  , _talkImage       :: Columnar f Text
  , _talkLanguages   :: Columnar f [Language]
  , _talkMediaSlug   :: Columnar f Text
  , _talkMediaPad    :: Columnar f Double
  } deriving (Beamable, Generic)

type Talk = TalkT Identity
type TalkId = PrimaryKey TalkT Identity

deriving instance Show Talk
deriving instance Eq Talk
deriving instance DB.FromRow Talk

instance ToJSON Talk where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = (\(x:xs) -> Char.toLower x : xs ) . drop 5 }

instance Table TalkT where
  data PrimaryKey TalkT f = TalkId (Columnar f Int) deriving Generic
  primaryKey = TalkId . _talkId
instance Beamable (PrimaryKey TalkT)

data TalkDb f = TalkDb
  { _talks :: f (TableEntity TalkT) }
  deriving Generic

instance Database be TalkDb

talkDb :: DatabaseSettings be TalkDb
talkDb = defaultDbSettings
