module View.Error where

import           Lucid
import           RIO
import           Servant (ServantErr (..), err404, throwError)
import           Types

get404H :: AppM (Html ())
get404H = do
  throwError err404 { errBody = renderBS page }
  -- liftIO $ throwError err404 { errBody = renderBS page }
  -- pure page
  where
  page = doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport"
            ,content_ "width=device-width, initial-scale=1"]
    body_ $ do
      div_ "I'm lost. The page you're looking for is not here."
