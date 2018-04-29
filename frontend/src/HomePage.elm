module HomePage exposing (Msg(..), Model, init, view)

import Array exposing (fromList, get)
import String exposing (split)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode
import Task
import CssModules exposing (css)
import Utils exposing (onPreventDefaultClick)
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


type Msg
    = RouteTo Route.Route


type alias Model =
    { talks : List Talk
    }


init : Task.Task Http.Error Model
init =
    Task.map Model <| Http.toTask getTalks


view : Model -> Html Msg
view model =
    div [ class .root ]
        [ div [ class .logo ] [ text ":: TED -> [SRT]" ]
        , SearchForm.view ""
        , div [ class .list ] (talksView model.talks)
        ]


talksView : List Talk -> List (Html Msg)
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
                        , onPreventDefaultClick (RouteTo route)
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
