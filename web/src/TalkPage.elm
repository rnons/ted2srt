module TalkPage exposing (..)

import Html exposing (..)
import Http
import Json.Decode as Decode
import CssModules exposing (css)
import Models.Talk exposing (Talk, talkDecoder)
import TalkPage.Header as TalkHeader
import TalkPage.Sidebar as Sidebar


{ class, classList } =
    css "./TalkPage/index.css"
        { root = ""
        , main = ""
        , sidebar = ""
        }


type alias Model =
    { slug : String
    , talk : Maybe Talk
    }


type Msg
    = TalkResult (Result Http.Error Talk)


init : String -> ( Model, Cmd Msg )
init slug =
    ( Model slug Nothing, getTalk slug )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TalkResult (Ok talk) ->
            ( { model | talk = Just talk }, Cmd.none )

        TalkResult (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.talk of
        Just talk ->
            div [ class .root ]
                [ main_ [ class .main ]
                    [ text <| "talk page, slug is " ++ model.slug
                    , TalkHeader.view talk
                    ]
                , aside []
                    [ Sidebar.view talk
                    ]
                ]

        _ ->
            div [] [ text "loading" ]


getTalk : String -> Cmd Msg
getTalk slug =
    let
        url =
            "/api/talks/" ++ slug
    in
        Http.send TalkResult (Http.get url talkDecoder)
