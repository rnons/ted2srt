module View.Home where

import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy  as LT
import           Lucid
import           Models.Talk     (getTalks)
import           RIO
import           Types
import           View.Bundle

getHomeH :: AppM (Html ())
getHomeH = do
  talks <- lift $ getTalks 0 20
  bundle <- includeBundle HomeBundle
  pure $ doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport"
            ,content_ "width=device-width, initial-scale=1"]
      script_ $ LT.toStrict $
        "window.TALKS = " <> encodeToLazyText talks
    body_ $ do
      bundle
