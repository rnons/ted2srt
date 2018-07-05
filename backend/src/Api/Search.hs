module Api.Search
  ( getSearchH
  ) where

import           Config
import qualified Data.Text                        as T
import qualified Database.PostgreSQL.Simple       as Pg
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Model
import           Models.Talk
import           Network.HTTP.Conduit             (HttpException)
import           RIO
import           Servant                          (err400)
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
  handle
    (\(e::HttpException) ->
       logErrorS "searchTalk" (displayShow e) >> searchTalkFromDb q
    ) $ do
    searchResults <- liftIO $ TED.searchTalk q
    liftM catMaybes $ forM searchResults $ \SearchTalk{slug} -> do
      getTalkBySlug slug

getSearchH :: Maybe Text -> AppM [Talk]
getSearchH (Just q) = searchTalk q
getSearchH Nothing  = throwM err400
