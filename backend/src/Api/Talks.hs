module Api.Talks
  ( getRandomTalkH
  , getTalkH
  , getTalksH
  ) where

import qualified Database.PostgreSQL.Simple       as Pg
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Models.Talk                      (getTalkBySlug, getTalks)
import           RIO
import           Servant                          (err404)
import           Types


getTalksH :: Maybe Int -> Maybe Int -> AppM [Talk]
getTalksH _ mLimit = do
  talks <- getTalks limit
  pure talks
  where
    defaultLimit = 10
    -- startTid = fromMaybe 0 mStartTid
    limit' = fromMaybe defaultLimit mLimit
    limit = if limit' > defaultLimit then defaultLimit else limit'

getRandomTalkH :: AppM Talk
getRandomTalkH = do
  Config { dbConn } <- ask
  xs <- liftIO $ Pg.query_ dbConn [sql|
      SELECT * FROM talk
      TABLESAMPLE SYSTEM (1)
      LIMIT 1
      |]
  case xs of
    [talk] -> pure talk
    _      -> throwM err404

getTalkH :: Text -> AppM Talk
getTalkH slug = do
  mTalk <- getTalkBySlug slug
  case mTalk of
    Just talk -> pure talk
    Nothing   -> throwM err404
