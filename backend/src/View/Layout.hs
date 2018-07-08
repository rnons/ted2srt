module View.Layout where

import           Lucid
import           RIO
import           Types

layout :: Html () -> Html () -> Html ()
layout headPartial bodyPartial =
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport"
            ,content_ "width=device-width, initial-scale=1"]
      headPartial
    body_ $ do
      bodyPartial
