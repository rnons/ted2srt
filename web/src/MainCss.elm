module MainCss exposing (..)

import Css exposing (..)
import Css.Colors exposing (red)
import Css.Namespace exposing (namespace)


type CssClasses
  = Input

css =
    (stylesheet << namespace "main")
    [ class Input
        [ focus [ border3 (px 1) solid red
                , outline none
            ]
        ]
    ]
