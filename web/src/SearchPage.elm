module SearchPage exposing (Model, init, title, view)

import Html exposing (..)
import Html.Attributes exposing (href, style)
import Http
import Json.Decode as Decode
import Task
import Models.Talk exposing (Talk, talkDecoder)
import CssModules exposing (css)
import Utils exposing (getDateString)
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


talkView : Talk -> Html msg
talkView talk =
    let
        talkUrl =
            "/talks/" ++ talk.slug
    in
        div [ class .item ]
            [ h3 []
                [ a [ href talkUrl ] [ text talk.name ]
                ]
            , div [ class .info ]
                [ a
                    [ class .cover
                    , style [ ( "backgroundImage", "url(" ++ talk.image ++ ")" ) ]
                    , href talkUrl
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


view : Model -> Html msg
view model =
    div []
        [ Header.view model.q
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
