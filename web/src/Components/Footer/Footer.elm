module Components.Footer.Footer exposing (Msg(..), view)

import Html exposing (..)
import Html.Attributes exposing (href, target)
import Html.Events exposing (onClick)
import CssModules exposing (css)


{ class } =
    css "./Components/Footer/index.css"
        { footer = ""
        }


type Msg
    = RandomTalk


view : Html Msg
view =
    footer [ class .footer ]
        [ text "TED2srt by "
        , a [ href "https://twitter.com/rnons", target "_blank" ] [ text "rnons" ]
        , text " | "
        , a [ onClick RandomTalk ] [ text "random talk" ]
        , text " | "
        , a [ href "https://github.com/rnons/ted2srt", target "_blank" ] [ text "source code" ]
        , text " | "
        , a [ href "/atom.xml", target "_blank" ] [ text "feed" ]
        ]
