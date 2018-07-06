module Search where

import Core.Prelude

import Effect (Effect)
import Effect.Console (error)
import Foreign (Foreign)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Search.App (app)
import Simple.JSON (read)

main :: Foreign -> Effect Unit
main f = case read f of
  Left e -> error $ show e
  Right pageData ->
    runHalogenAff do
      body <- awaitBody
      runUI (app pageData) unit body
