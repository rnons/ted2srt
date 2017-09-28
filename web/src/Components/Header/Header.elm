module Components.Header.Header exposing (view)

import Html exposing (..)
import Html.Attributes exposing (href)
import Components.SearchForm.SearchForm as SearchForm
import CssModules exposing (css)


{ class } =
    css "./Components/Header/index.css"
        { header = ""
        , container = ""
        , logo = ""
        }


view : String -> Html msg
view q =
    div [ class .header ]
        [ div [ class .container ]
            [ a [ class .logo, href "/" ]
                [ text ":: TED -> [SRT]"
                ]
            , SearchForm.view q
            ]
        ]
