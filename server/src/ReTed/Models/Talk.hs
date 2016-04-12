{-# LANGUAGE DeriveGeneric #-}
module ReTed.Models.Talk where

import           Control.Monad (mzero, liftM, void)
import           Data.Aeson
import           Data.Text (Text)
import           qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Database.PostgreSQL.Simple as DB
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit (simpleHttp)
import           Prelude hiding (id)

import Web.TED.TalkPage (parseTalkObject)


data Language = Language
    { languageName :: Text
    , languageCode :: Text
    } deriving (Generic, Show)
instance FromJSON Language


data Talk = Talk
    { id           :: Int
    , name         :: Text
    , slug         :: Text
    , filmedAt     :: UTCTime
    , publishedAt  :: UTCTime
    , languages    :: [Language]
    } deriving (Generic, Show)

instance FromJSON Talk where
    parseJSON (Object v) =
        Talk <$> v .: "id"
             <*> v .: "name"
             <*> v .: "slug"
             <*> liftM posixSecondsToUTCTime (v .: "filmed")
             <*> liftM posixSecondsToUTCTime (v .: "published")
             <*> v .: "languages"
    parseJSON _          = mzero


data Talks = Talks
    { talks :: [Talk]
    } deriving (Generic, Show)

instance FromJSON Talks


saveToDB :: DB.Connection -> Int -> Text -> IO ()
saveToDB conn tid url = do
    print (tid, url)
    Talk {id, name, slug, filmedAt, publishedAt}  <- fetchTalk tid url
    void $ DB.execute conn [sql|
        INSERT INTO talks (id, name, slug, filmed, published)
        VALUES (?, ?, ?, ?, ?)
        |] (id, name, slug, filmedAt, publishedAt)

fetchTalk :: Int -> Text -> IO Talk
fetchTalk tid url = do
    body <- simpleHttp $ T.unpack url
    let core = parseTalkObject body
    case decode core of
        Just tks -> do
            print $ name $ head $ talks tks
            return $ head $ talks tks
        _      -> error "parse error"
