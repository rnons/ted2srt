module Home where

import Core.Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Home.App (app)

main :: Json -> Effect Unit
main f = case decodeJson f of
  Left e -> throw e
  Right pageData ->
    runHalogenAff do
      body <- awaitBody
      runUI (app pageData) unit body
