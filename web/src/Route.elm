module Route exposing (..)

import Navigation exposing (Location)
import String
import UrlParser as Url exposing ((</>), (<?>), Parser, oneOf, parseHash, parsePath, s, string, stringParam)


type Route
    = Home
    | Talk String


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Talk (s "talks" </> string)
        ]


fromLocation : Location -> Maybe Route
fromLocation location =
    parsePath route location
