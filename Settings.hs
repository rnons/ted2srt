{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Settings where

import Text.Cassius
import Text.Julius
import Yesod.Static

staticDir :: FilePath
staticDir = "static"

staticSite :: IO Static
#if PRODUCTION 
staticSite = static staticDir
cssFile = cassiusFile
jssFile = juliusFile
#else
staticSite = staticDevel staticDir
cssFile = cassiusFileReload
jssFile = juliusFileReload
#endif

