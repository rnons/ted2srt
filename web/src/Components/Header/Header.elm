module Components.Header.Header exposing (Msg(..), view)

import Html exposing (..)
import Html.Attributes exposing (href)
import Components.SearchForm.SearchForm as SearchForm
import CssModules exposing (css)
import Utils exposing (onPreventDefaultClick)
import Route


{ class } =
    css "./Components/Header/index.css"
        { header = ""
        , container = ""
        , logo = ""
        }


type Msg
    = RouteTo Route.Route


view : String -> Html Msg
view q =
    div [ class .header ]
        [ div [ class .container ]
            [ a
                [ class .logo
                , href "/"
                , onPreventDefaultClick (RouteTo Route.Home)
                ]
                [ text ":: TED -> [SRT]"
                ]
            , SearchForm.view q
            ]
        ]
