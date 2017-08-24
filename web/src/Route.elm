module Route exposing (..)

import Navigation exposing (Location)
import String
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)


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
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash route location
