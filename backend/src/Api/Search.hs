module Api.Search
  ( getSearchApiH
  ) where

import qualified Data.Text        as T
import           Database.Persist (Entity, (||.))
import qualified Database.Persist as P
import           Model
import           RIO
import           Servant          (err400, throwError)
import           Types            (AppM, AppRIO, runDB)


-- Based on https://stackoverflow.com/a/11069667
ilike :: EntityField record Text -> Text -> P.Filter record
ilike field q =
  P.Filter field (Left query) (P.BackendSpecificFilter "ilike")
  where
    q' = T.map (\c -> if c == '_' then ' ' else c) q
    query = "%" <> T.intercalate "%" (T.words q') <> "%"

searchTalk :: Text -> AppRIO [Entity Talk]
searchTalk q = do
  runDB $ P.selectList ([TalkName `ilike` q] ||. [TalkDescription `ilike` q]) []

getSearchApiH :: Maybe Text -> AppM [Entity Talk]
getSearchApiH (Just q) = lift $ searchTalk q
getSearchApiH Nothing  = throwError err400
