module TalkPage exposing (..)

import Html exposing (..)
import Http
import Json.Decode as Decode
import Set
import CssModules exposing (css)
import Models.Talk exposing (Talk, LanguageCode, talkDecoder)
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
    , selectedLangs : Set.Set LanguageCode
    , transcript : String
    }


type Msg
    = TalkResult (Result Http.Error Talk)
    | Transcript (Result Http.Error String)
    | Sidebar Sidebar.Msg


init : String -> ( Model, Cmd Msg )
init slug =
    ( { slug = slug
      , talk = Nothing
      , selectedLangs = Set.empty
      , transcript = ""
      }
    , getTalk slug
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TalkResult (Ok talk) ->
            ( { model | talk = Just talk }, Cmd.none )

        TalkResult (Err _) ->
            ( model, Cmd.none )

        Transcript (Ok transcript) ->
            ( { model | transcript = transcript }, Cmd.none )

        Transcript (Err _) ->
            ( model, Cmd.none )

        Sidebar (Sidebar.SelectLang lang) ->
            let
                newSet =
                    if Set.member lang.code model.selectedLangs then
                        Set.remove lang.code model.selectedLangs
                    else if Set.size model.selectedLangs < 2 then
                        Set.insert lang.code model.selectedLangs
                    else
                        model.selectedLangs
            in
                ( { model | selectedLangs = newSet }, getTranscript model.talk lang.code )


view : Model -> Html Msg
view model =
    case model.talk of
        Just talk ->
            div [ class .root ]
                [ main_ [ class .main ]
                    [ text <| "talk page, slug is " ++ model.slug
                    , TalkHeader.view talk
                    , text model.transcript
                    ]
                , aside []
                    [ Sidebar.view talk model.selectedLangs |> Html.map Sidebar
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


getTranscript : Maybe Talk -> LanguageCode -> Cmd Msg
getTranscript mtalk code =
    case mtalk of
        Just talk ->
            let
                url =
                    "/api/talks/" ++ toString talk.id ++ "/transcripts/txt?lang=" ++ code
            in
                Http.send Transcript (Http.getString url)

        Nothing ->
            Cmd.none
