module Route exposing (..)

import Navigation exposing (Location)
import String
import UrlParser as Url exposing ((</>), (<?>), Parser, oneOf, parsePath, s, string, stringParam)


type Route
    = Home
    | Talk String
    | Search (Maybe String)


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Talk (s "talks" </> string)
        , Url.map Search (s "search" <?> stringParam "q")
        ]


fromLocation : Location -> Maybe Route
fromLocation location =
    parsePath route location
