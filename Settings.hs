{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Settings where

import Language.Haskell.TH.Syntax
import Text.Cassius
import Text.Hamlet
import Text.Julius
import Yesod.Static
import Data.Default
import Yesod.Default.Util

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

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

widgetFile :: String -> Q Exp
#if PRODUCTION 
widgetFile = widgetFileNoReload widgetFileSettings
#else
widgetFile = widgetFileReload widgetFileSettings
#endif
