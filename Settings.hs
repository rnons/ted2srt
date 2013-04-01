{-# LANGUAGE TemplateHaskell #-}

module Settings where

import Yesod.Static

staticDir :: FilePath
staticDir = "static"

staticSite :: IO Static
staticSite = static staticDir

