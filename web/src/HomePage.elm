module HomePage exposing (..)

import Array exposing (fromList, get)
import String exposing (split)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode
import CssModules exposing (css)
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
    { url : String
    , talks : List Talk
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" [], getTalks )


type Msg
    = Input String
    | Submit
    | TalksResult (Result Http.Error (List Talk))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input text ->
            ( { model | url = text }, Cmd.none )

        Submit ->
            ( { model | url = model.url ++ " SUBMITTED" }, Cmd.none )

        TalksResult (Ok talks) ->
            ( { model | talks = talks }, Cmd.none )

        TalksResult (Err _) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [ class .root ]
        [ div [ class .logo ] [ text ":: TED -> [SRT]" ]
        , SearchForm.view
        , div [ class .list ] (talksView model.talks)
        ]


talksView : List Talk -> List (Html Msg)
talksView talks =
    talks
        |> List.map
            (\talk ->
                a
                    [ class .tile
                    , href ("/talks/" ++ talk.slug)
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


getTalks : Cmd Msg
getTalks =
    let
        url =
            "/api/talks?limit=5"
    in
        Http.send TalksResult (Http.get url <| Decode.list talkDecoder)


decodeTalks : Decode.Decoder (List Talk)
decodeTalks =
    Decode.list talkDecoder
