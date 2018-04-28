{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Model where

import           Data.Aeson
import qualified Data.Char                            as Char
import           Data.Time                            (UTCTime)
import           Database.Beam
import           Database.Beam.Backend.SQL.SQL92      (HasSqlValueSyntax (..))
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax        (PgValueSyntax (..),
                                                       pgBuildAction)
import qualified Database.PostgreSQL.Simple           as DB
import qualified Database.PostgreSQL.Simple.FromField as DB
import qualified Database.PostgreSQL.Simple.ToField   as DB
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

instance DB.ToField [Language] where
    toField = DB.toJSONField

instance FromBackendRow Postgres [Language]
instance HasSqlValueSyntax PgValueSyntax [Language] where
  sqlValueSyntax =
    PgValueSyntax . pgBuildAction . pure . DB.toField

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

deriving instance Show TalkId

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

talkDb :: DatabaseSettings be TalkDb
talkDb = defaultDbSettings `withDbModification`
  ( dbModification
    { _transcripts = modifyTable (\_ -> "transcripts")
      (Transcript (TalkId "id") "en_tsvector")
    }
  )
