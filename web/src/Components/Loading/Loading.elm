module Components.Loading.Loading exposing (view)

import Html exposing (Html, div)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (class)
import CssModules exposing (css)


{ toString } =
    css "./Components/Loading/index.css"
        { root = ""
        , loading = ""
        , step = ""
        }


view : Html msg
view =
    div [ class (toString .root) ]
        [ svg [ class (toString .loading) ] <|
            List.map
                (\_ -> rect [ class (toString .step) ] [])
                (List.range 1 12)
        ]
