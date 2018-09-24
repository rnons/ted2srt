module Models.Types where

import           RIO

mkTalkUrl :: Text -> Text
mkTalkUrl s = "http://www.ted.com/talks/" <> s
