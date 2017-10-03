module NotFoundPage exposing (view)

import Html exposing (..)
import Html.Attributes exposing (style)
import Components.Header.Header as Header


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
            [ text "Sorry, we cannot find anything" ]
        ]
