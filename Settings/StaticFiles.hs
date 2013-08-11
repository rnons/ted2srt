{-# LANGUAGE TemplateHaskell #-}
module Settings.StaticFiles where

import Yesod.Static
import Settings (staticDir)

$(staticFiles staticDir)
