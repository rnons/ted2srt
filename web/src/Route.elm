module Route exposing (Route(..), fromLocation, toString, talkToRoute)

import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), (<?>), Parser, oneOf, parsePath, s, string, stringParam)
import Models.Talk exposing (Talk)


type Route
    = Home
    | Talk String
    | Search (Maybe String)


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Url.map Home (s "")
        , Url.map Talk (s "talks" </> string)
        , Url.map Search (s "search" <?> stringParam "q")
        ]


fromLocation : Location -> Maybe Route
fromLocation location =
    parsePath routeParser location


toString : Route -> String
toString route =
    case route of
        Home ->
            "/"

        Talk slug ->
            "/talks/" ++ slug

        Search (Just q) ->
            "/search?q" ++ q

        Search Nothing ->
            "/"


talkToRoute : Talk -> Route
talkToRoute talk =
    Talk talk.slug
