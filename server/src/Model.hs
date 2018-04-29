{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
module Model where

import           Data.Aeson
import qualified Data.Char                       as Char
import           Data.Time                       (UTCTime)
import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92 (timestampType)
import           Database.Beam.Migrate           (CheckedDatabaseSettings,
                                                  HasDefaultSqlDataType (..),
                                                  HasDefaultSqlDataTypeConstraints,
                                                  IsSql92ColumnSchemaSyntax,
                                                  defaultMigratableDbSettings,
                                                  modifyCheckedTable,
                                                  unCheckDatabase)
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax   (PgDataTypeSyntax)
import qualified Database.PostgreSQL.Simple      as Pg
import           RIO

data Language = Language
  { languageName :: Text
  , languageCode :: Text
  , endonym      :: Text
  } deriving (Generic, Eq, Show)
instance FromJSON Language
instance ToJSON Language

instance HasDefaultSqlDataType PgDataTypeSyntax UTCTime where
  defaultSqlDataType _ _ = timestampType Nothing True

instance IsSql92ColumnSchemaSyntax syn => HasDefaultSqlDataTypeConstraints syn UTCTime

data TalkT f = Talk
  { _talkId          :: Columnar f Int
  , _talkName        :: Columnar f Text
  , _talkSlug        :: Columnar f Text
  , _talkFilmedAt    :: Columnar f UTCTime
  , _talkPublishedAt :: Columnar f UTCTime
  , _talkDescription :: Columnar f Text
  , _talkImage       :: Columnar f Text
  , _talkLanguages   :: Columnar f (PgJSONB [Language])
  , _talkMediaSlug   :: Columnar f Text
  , _talkMediaPad    :: Columnar f Double
  } deriving (Beamable, Generic)

type Talk = TalkT Identity
type TalkId = PrimaryKey TalkT Identity

deriving instance Show Talk
deriving instance Eq Talk
deriving instance Pg.FromRow Talk

deriving instance Show TalkId

instance ToJSON (PgJSONB [Language]) where
  toJSON (PgJSONB langs) = toJSON langs

instance ToJSON Talk where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = (\(x:xs) -> Char.toLower x : xs ) . drop 5 }

instance Table TalkT where
  data PrimaryKey TalkT f = TalkId (Columnar f Int) deriving Generic
  primaryKey = TalkId . _talkId
instance Beamable (PrimaryKey TalkT)


data TranscriptT f = Transcript
  { _transcriptTalk       :: PrimaryKey TalkT f
  , _transcriptEnTsvector :: Columnar f TsVector
  } deriving (Beamable, Generic)

type Transcript = TranscriptT Identity
type TranscriptId = PrimaryKey TranscriptT Identity

deriving instance Show Transcript

instance Table TranscriptT where
  data PrimaryKey TranscriptT f = TranscriptId (PrimaryKey TalkT f) deriving Generic
  primaryKey = TranscriptId . _transcriptTalk
instance Beamable (PrimaryKey TranscriptT)


data TalkDb f = TalkDb
  { _talks       :: f (TableEntity TalkT)
  , _transcripts :: f (TableEntity TranscriptT)
  } deriving Generic

instance Database be TalkDb

talkDbMigration :: CheckedDatabaseSettings Postgres TalkDb
talkDbMigration =
  (defaultMigratableDbSettings @PgCommandSyntax) `withDbModification`
  ( dbModification
    { _transcripts = modifyCheckedTable (\_ -> "transcripts")
      (Transcript (TalkId "id") "en_tsvector")
    }
  )

talkDb :: DatabaseSettings Postgres TalkDb
talkDb = unCheckDatabase talkDbMigration
