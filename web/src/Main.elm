module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode
import CssModules exposing (css)


{ class, classList } =
    css "./main.css"
        { input = ""
        , util = ""
        }


main =
    program { init = init, view = view, update = update, subscriptions = subscriptions }


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
    div []
        [ Html.form [ onSubmit Submit ]
            [ input
                [ type_ "text"
                , classList [ ( .input, True ), ( .util, True ) ]
                , value model.url
                , placeholder "TED talk url or keywords"
                , onInput Input
                ]
                []
            , input
                [ type_ "submit"
                , hidden True
                ]
                []
            ]
        , div [] (talksView model.talks)
        ]


talksView : List Talk -> List (Html Msg)
talksView talks =
    talks
        |> List.map (\talk -> div [] [ text talk.name ])


type alias Talk =
    { name : String
    }


getTalks : Cmd Msg
getTalks =
    let
        url =
            "/api/talks?limit=5"
    in
        Http.send TalksResult (Http.get url decodeTalks)


talkDecoder =
    Decode.map Talk (Decode.field "name" Decode.string)


decodeTalks : Decode.Decoder (List Talk)
decodeTalks =
    Decode.list talkDecoder
