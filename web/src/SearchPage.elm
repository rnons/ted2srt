module SearchPage exposing (Msg(..), Model, init, title, view)

import Html exposing (..)
import Html.Attributes exposing (href, style)
import Http
import Json.Decode as Decode
import Task
import Route
import Models.Talk exposing (Talk, talkDecoder)
import CssModules exposing (css)
import Utils exposing (getDateString, onPreventDefaultClick)
import Components.Header.Header as Header


{ class, classList } =
    css "./SearchPage/index.css"
        { root = ""
        , item = ""
        , info = ""
        , cover = ""
        , description = ""
        , date = ""
        }


type Msg
    = RouteTo Route.Route


type alias Model =
    { q : String
    , talks : List Talk
    }


init : String -> Task.Task Http.Error Model
init q =
    Http.toTask (searchTalk q) |> Task.map (\talks -> { q = q, talks = talks })


title : Model -> String
title model =
    model.q ++ " - TED2srt search"


talkView : Talk -> Html Msg
talkView talk =
    let
        route =
            Route.talkToRoute talk

        talkUrl =
            Route.toString route
    in
        div [ class .item ]
            [ h3 []
                [ a
                    [ href talkUrl
                    , onPreventDefaultClick (RouteTo route)
                    ]
                    [ text talk.name ]
                ]
            , div [ class .info ]
                [ a
                    [ class .cover
                    , style [ ( "backgroundImage", "url(" ++ talk.image ++ ")" ) ]
                    , href talkUrl
                    , onPreventDefaultClick (RouteTo route)
                    ]
                    []
                , div []
                    [ p []
                        [ text talk.description
                        , span [ class .date ] [ text ("Published: " ++ getDateString talk.publishedAt) ]
                        ]
                    ]
                ]
            ]


view : Model -> Html Msg
view model =
    div []
        [ Header.view model.q |> Html.map (\(Header.RouteTo route) -> RouteTo route)
        , div [ class .root ]
            (List.map talkView model.talks)
        ]


searchTalk : String -> Http.Request (List Talk)
searchTalk q =
    let
        url =
            "/api/search?q=" ++ q
    in
        Http.get url <| Decode.list talkDecoder
