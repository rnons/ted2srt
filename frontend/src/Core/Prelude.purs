module Core.Prelude
  ( module Prelude
  , module Data.Either
  , module Data.Maybe
  , class_
  , style
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

class_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
class_ = HP.class_ <<< HH.ClassName

style :: forall r i. String -> HP.IProp ("style" :: String | r) i
style = HP.attr (HH.AttrName "style")
