module HomePage exposing (Model, init, view)

import Array exposing (fromList, get)
import String exposing (split)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode
import Task
import CssModules exposing (css)
import Route
import Models.Talk exposing (..)
import Components.SearchForm.SearchForm as SearchForm


{ class, classList } =
    css "./HomePage/index.css"
        { root = ""
        , logo = ""
        , list = ""
        , tile = ""
        , image = ""
        , info = ""
        , title = ""
        , speaker = ""
        }


type alias Model =
    { talks : List Talk
    }


init : Task.Task Http.Error Model
init =
    Task.map Model <| Http.toTask getTalks


view : Model -> Html msg
view model =
    div [ class .root ]
        [ div [ class .logo ] [ text ":: TED -> [SRT]" ]
        , SearchForm.view ""
        , div [ class .list ] (talksView model.talks)
        ]


talksView : List Talk -> List (Html msg)
talksView talks =
    talks
        |> List.map
            (\talk ->
                let
                    route =
                        Route.talkToRoute talk
                in
                    a
                        [ class .tile
                        , href (Route.toString route)
                        ]
                        [ div
                            [ class .image
                            , style [ ( "backgroundImage", "url(" ++ talk.image ++ ")" ) ]
                            ]
                            []
                        , div
                            [ class .info ]
                            [ div [ class .title ] [ text talk.title ]
                            , div [ class .speaker ] [ text talk.speaker ]
                            ]
                        ]
            )


getTalks : Http.Request (List Talk)
getTalks =
    let
        url =
            "/api/talks?limit=5"
    in
        Http.get url <| Decode.list talkDecoder
