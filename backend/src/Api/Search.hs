module Api.Search
  ( getSearchApiH
  ) where

import           Config
import qualified Data.Text                        as T
import qualified Database.PostgreSQL.Simple       as Pg
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Model
import           RIO
import           Servant                          (err400, throwError)
import           Types                            (AppM)

searchTalkFromDb :: Text -> AppM [Talk]
searchTalkFromDb q = do
  Config { dbConn } <- ask
  liftIO $ Pg.query dbConn [sql|
      SELECT talk.* FROM talk JOIN transcript
      ON talk.id = transcript.id
      WHERE en_tsvector @@
            to_tsquery('english', ?)
      |] [query]
  where
    query = T.intercalate "&" $ T.words q

searchTalk :: Text -> AppM [Talk]
searchTalk = searchTalkFromDb

getSearchApiH :: Maybe Text -> AppM [Talk]
getSearchApiH (Just q) = searchTalk q
getSearchApiH Nothing  = throwError err400
