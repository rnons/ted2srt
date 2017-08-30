module Components.SearchForm.SearchForm exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


view : Html msg
view =
    Html.form
        [ method "GET"
        , action "/search"
        ]
        [ input
            [ type_ "text"
            , name "q"
            , placeholder "TED talk url or keywords"
            ]
            []
        , input
            [ type_ "submit"
            , hidden True
            ]
            []
        ]
