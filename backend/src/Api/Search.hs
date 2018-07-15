module Api.Search
  ( getSearchApiH
  ) where

import           Config
import           Control.Monad.Except             (catchError)
import qualified Data.Text                        as T
import qualified Database.PostgreSQL.Simple       as Pg
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Model
import           Models.Talk
import           RIO
import           Servant                          (err400, throwError)
import           Types                            (AppM)
import           Web.TED                          (SearchTalk (..))
import qualified Web.TED                          as TED

searchTalkFromDb :: Text -> AppM [Talk]
searchTalkFromDb q = do
  Config { dbConn } <- ask
  liftIO $ Pg.query dbConn [sql|
      SELECT talks.* FROM talks JOIN transcripts
      ON talks.id = transcripts.id
      WHERE en_tsvector @@
            to_tsquery('english', ?)
      |] [query]
  where
    query = T.intercalate "&" $ T.words q

searchTalk :: Text -> AppM [Talk]
searchTalk q = do
  (do
    searchResults <- liftIO $ TED.searchTalk q
    liftM catMaybes $ forM searchResults $ \SearchTalk{slug} -> do
      lift $ getTalkBySlug slug
    ) `catchError`
    (\e ->
      logErrorS "searchTalk" (displayShow e) >> searchTalkFromDb q
    )

getSearchApiH :: Maybe Text -> AppM [Talk]
getSearchApiH (Just q) = searchTalk q
getSearchApiH Nothing  = throwError err400
