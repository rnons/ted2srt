module Api.Talks
  ( getRandomTalkApiH
  , getTalkApiH
  , getTalksApiH
  ) where

import qualified Database.PostgreSQL.Simple       as Pg
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Models.Talk                      (getTalkBySlug, getTalks)
import           RIO
import           Servant                          (err404, throwError)
import           Types


getTalksApiH :: Maybe Int -> Maybe Int -> AppM [Talk]
getTalksApiH _ mLimit = do
  talks <- lift $ getTalks limit
  pure talks
  where
    defaultLimit = 10
    -- startTid = fromMaybe 0 mStartTid
    limit' = fromMaybe defaultLimit mLimit
    limit = if limit' > defaultLimit then defaultLimit else limit'

getRandomTalkApiH :: AppM Talk
getRandomTalkApiH = do
  Config { dbConn } <- ask
  xs <- liftIO $ Pg.query_ dbConn [sql|
      SELECT * FROM talk
      TABLESAMPLE SYSTEM (1)
      LIMIT 1
      |]
  case xs of
    [talk] -> pure talk
    _      -> throwM err404

getTalkApiH :: Text -> AppM Talk
getTalkApiH slug = do
  mTalk <- lift $ getTalkBySlug slug
  case mTalk of
    Just talk -> pure talk
    Nothing   -> throwError err404
