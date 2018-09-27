module Web.TED.Types
  ( Cue(..)
  , Paragraph(..)
  , Transcript(..)
  , transcriptToText
  ) where


import           Data.Aeson
import           Data.Text  (Text)
import qualified Data.Text  as T
import           RIO

data Cue = Cue
    { time :: Int
    , text :: Text
    } deriving (Generic, Show)
instance FromJSON Cue

data Paragraph = Paragraph
    { cues :: [Cue]
    } deriving (Generic, Show)
instance FromJSON Paragraph

data Transcript = Transcript
    { paragraphs :: [Paragraph]
    } deriving (Generic, Show)
instance FromJSON Transcript

transcriptToText :: Transcript -> Text
transcriptToText (Transcript ps) =
    T.intercalate "\n" $ map (
      \(Paragraph cues) -> T.intercalate " " $ map (T.replace "\n" " " . text) cues) ps
