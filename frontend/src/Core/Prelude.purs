module Core.Prelude
  ( module Prelude
  , module Data.Either
  , module Data.Foldable
  , module Data.Maybe
  , module Data.Tuple
  , module Debug.Trace
  , class_
  , style
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (for_, traverse_)
import Debug.Trace (trace, traceM)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

class_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
class_ = HP.class_ <<< HH.ClassName

style :: forall r i. String -> HP.IProp ("style" :: String | r) i
style = HP.attr (HH.AttrName "style")
