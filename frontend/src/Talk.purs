module Talk where

import Core.Prelude

import Effect (Effect)
import Effect.Console (error)
import Foreign (Foreign)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Simple.JSON (read)
import Talk.App (app)

main :: Foreign -> Effect Unit
main f = case read f of
  Left e -> error $ show e
  Right pageData ->
    runHalogenAff do
      body <- awaitBody
      runUI (app pageData) unit body
