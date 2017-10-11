module ErrorPage exposing (title, view)

import Html exposing (..)
import Html.Attributes exposing (style, href)
import Components.Header.Header as Header


title : String
title =
    "Server error - TED2srt"


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
            [ p [] [ text "It's embarassing to say, but an error just happened." ]
            , p []
                [ text "Please try again, or "
                , a [ href "https://twitter.com/rnons" ] [ text "contact me" ]
                , text "."
                ]
            ]
        ]
