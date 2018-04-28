module Handler.Search
  ( getSearchH
  ) where

import           Config
import qualified Data.Text                        as T
import qualified Database.PostgreSQL.Simple       as Pg
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Model
import           Models.Talk
import           Network.HTTP.Conduit             (HttpException)
import           RIO                              hiding (Handler)
import           Servant                          (Handler, err400, throwError)
import           System.IO                        (print)
import           Web.TED                          (SearchTalk (..))
import qualified Web.TED                          as TED

searchTalkFromDb :: Config -> Text -> IO [Talk]
searchTalkFromDb config q = do
    Pg.query conn [sql|
        SELECT talks.* FROM talks JOIN transcripts
        ON talks.id = transcripts.id
        WHERE en_tsvector @@
              to_tsquery('english', ?)
        |] [query]
  where
    conn = dbConn config
    query = T.intercalate "&" $ T.words q

searchTalk :: Config -> Text -> IO [Talk]
searchTalk config q =
  handle (\(e::HttpException) -> print e >> searchTalkFromDb config q) $ do
    searchResults <- TED.searchTalk q
    liftM catMaybes $ forM searchResults $ \SearchTalk{slug} -> do
      getTalkBySlug config slug

getSearchH :: Config -> Maybe Text -> Handler [Talk]
getSearchH config (Just q) = liftIO $ searchTalk config q
getSearchH _ Nothing       = throwError err400
