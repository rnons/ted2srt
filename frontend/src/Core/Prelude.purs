module Core.Prelude
  ( module Prelude
  , module Control.MonadPlus
  , module Data.Either
  , module Data.Foldable
  , module Data.Maybe
  , module Data.Symbol
  , module Data.Tuple
  , module Debug.Trace
  , module Effect.Aff
  , class_
  , svgClass_
  , style
  ) where

import Prelude

import Control.MonadPlus (guard)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (for_, traverse_)
import Debug.Trace (trace, traceM)
import Effect.Aff (Aff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

class_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
class_ = HP.class_ <<< HH.ClassName

svgClass_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
svgClass_ = HP.attr (HH.AttrName "class")

style :: forall r i. String -> HP.IProp ("style" :: String | r) i
style = HP.attr (HH.AttrName "style")
