module View.Bundle where

import qualified Data.Text as T
import           Lucid
import           RIO
import           Types

data Bundle
  = HomeBundle
  | TalkBundle

instance Show Bundle where
  show = \case
    HomeBundle -> "Home"
    TalkBundle -> "Talk"

includeBundle :: Bundle -> AppM (Html ())
includeBundle bundle = lift $ do
  Config { devMode, lookupStatic } <- ask
  let
    (commonJs, bundleJs) = if devMode
      then
        (devPrefix <> "common.js", devPrefix <> show bundle <> ".js")
      else
        (distPrefix <> "common.js", distPrefix <> lookupStatic (show bundle) <> ".js")
  pure $ do
    script_ [src_ $ T.pack commonJs] ("" :: Text)
    script_ [src_ $ T.pack bundleJs] ("" :: Text)
  where
  devPrefix = "http://localhost:7000/"
  distPrefix = "/dist/"
