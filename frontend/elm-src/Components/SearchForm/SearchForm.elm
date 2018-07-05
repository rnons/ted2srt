module Components.SearchForm.SearchForm exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import CssModules exposing (css)


{ class } =
    css "./Components/SearchForm/index.css"
        { form = ""
        , input = ""
        , sidebar = ""
        , row = ""
        }


view : String -> Html msg
view q =
    Html.form
        [ class .form
        , method "GET"
        , action "/search"
        ]
        [ input
            [ class .input
            , type_ "text"
            , name "q"
            , value q
            , required True
            , placeholder "TED talk url or keywords"
            ]
            []
        , input
            [ type_ "submit"
            , hidden True
            ]
            []
        ]
