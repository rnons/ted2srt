module View.Bundle where

import qualified Data.Text as T
import           Lucid
import           RIO
import           Types

data Bundle
  = HomeBundle
  | TalkBundle
  | SearchBundle

instance Show Bundle where
  show = \case
    HomeBundle -> "Home"
    TalkBundle -> "Talk"
    SearchBundle -> "Search"

includeBundle :: Bundle -> AppM (Html ())
includeBundle bundle = do
  Config { devMode, lookupStatic } <- ask
  let
    prefix = if devMode then "http://localhost:7000/" else "/dist/"
    [commonJs, commonCss, bundleJs, bundleCss] = (prefix <>) <$> if devMode
      then
        ["common.js", "", show bundle <> ".js", ""]
      else
        [ lookupStatic $ "common" <> ".js"
        , lookupStatic $ "common" <> ".css"
        , lookupStatic $ show bundle <> ".js"
        , lookupStatic $ show bundle <> ".css"
        ]
  pure $ do
    when (not devMode) $ do
      link_ [ href_ $ T.pack commonCss, rel_ "stylesheet" ]
      link_ [ href_ $ T.pack bundleCss, rel_ "stylesheet" ]
    script_ [ src_ $ T.pack commonJs ] ("" :: Text)
    script_ [ src_ $ T.pack bundleJs ] ("" :: Text)
