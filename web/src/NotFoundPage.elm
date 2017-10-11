module NotFoundPage exposing (title, view)

import Html exposing (..)
import Html.Attributes exposing (style)
import Components.Header.Header as Header


title : String
title =
    "404 error - TED2srt"


view : Html msg
view =
    div []
        [ Header.view ""
        , div
            [ style
                [ ( "max-width", "64rem" )
                , ( "margin", "0 auto" )
                , ( "padding", "1.25rem 0" )
                ]
            ]
            [ text "Sorry, we cannot find anything." ]
        ]
