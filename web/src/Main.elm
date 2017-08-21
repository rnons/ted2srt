module Main exposing (..)

import Array exposing (fromList, get)
import String exposing (split)
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
        , root = ""
        , list = ""
        , tile = ""
        , image = ""
        , info = ""
        , title = ""
        , speaker = ""
        }


main =
    program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Talk =
    { slug : String
    , image : String
    , title : String
    , speaker : String
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
        , div [ class .list ] (talksView model.talks)
        ]


talksView : List Talk -> List (Html Msg)
talksView talks =
    talks
        |> List.map
            (\talk ->
                a [ class .tile ]
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
        Http.send TalksResult (Http.get url decodeTalks)


talkDecoder : Decode.Decoder Talk
talkDecoder =
    Decode.field "name" Decode.string |> Decode.andThen talkNameDecoder


talkNameDecoder : String -> Decode.Decoder Talk
talkNameDecoder name =
    let
        array =
            fromList <| split ":" name

        speaker =
            get 0 array

        title =
            get 1 array
    in
        Decode.map4 Talk
            (Decode.field "slug" Decode.string)
            (Decode.map (String.join "&" << String.split "&amp;") <| Decode.field "image" Decode.string)
            (Decode.succeed <| Maybe.withDefault name title)
            (Decode.succeed <| Maybe.withDefault name speaker)


decodeTalks : Decode.Decoder (List Talk)
decodeTalks =
    Decode.list talkDecoder
