module Talk where

import Prelude

import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Talk.App (app)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI app unit body
