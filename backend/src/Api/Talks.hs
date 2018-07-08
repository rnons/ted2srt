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
getTalksApiH mOffset mLimit = do
  talks <- lift $ getTalks offset limit
  pure talks
  where
  defaultLimit = 20
  maxLimit = 20
  offset = fromMaybe 0 mOffset
  limit = maybe defaultLimit (\i -> if i > maxLimit then maxLimit else i) mLimit

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
