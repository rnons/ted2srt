module Api.Talks
  ( getRandomTalkApiH
  , getTalkApiH
  , getTalksApiH
  ) where

import           Database.Persist (Entity (..))
import           Models.Talk      (getTalkBySlug, getTalks)
import           RIO
import           Servant          (err404, throwError)
import           Types


getTalksApiH :: Maybe Int -> Maybe Int -> AppM [Entity Talk]
getTalksApiH mOffset mLimit = do
  lift $ getTalks offset limit
  where
  defaultLimit = 20
  maxLimit = 20
  offset = fromMaybe 0 mOffset
  limit = maybe defaultLimit (\i -> if i > maxLimit then maxLimit else i) mLimit

getRandomTalkApiH :: AppM Talk
getRandomTalkApiH = do
  let xs = []
  -- xs <- liftIO $ Pg.query_ dbConn [sql|
  --     SELECT * FROM talk
  --     TABLESAMPLE SYSTEM (1)
  --     LIMIT 1
  --     |]
  case xs of
    [talk] -> pure $ entityVal talk
    _      -> throwM err404

getTalkApiH :: Text -> AppM Talk
getTalkApiH slug = do
  mTalk <- lift $ getTalkBySlug slug
  case mTalk of
    Just talk -> pure $ entityVal talk
    Nothing   -> throwError err404
