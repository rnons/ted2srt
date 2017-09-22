module TalkPage exposing (..)

import Html exposing (..)
import Http
import Json.Encode as Encode
import Json.Decode as Decode
import Dict
import Set
import CssModules exposing (css)
import LocalStorage
import Models.Talk exposing (Talk, LanguageCode, talkDecoder)
import Components.Header.Header as Header
import TalkPage.Header as TalkHeader
import TalkPage.Sidebar as Sidebar


{ class, classList } =
    css "./TalkPage/index.css"
        { root = ""
        , main = ""
        , sidebar = ""
        , row = ""
        }


type alias Transcript =
    String


type alias Model =
    { slug : String
    , talk : Maybe Talk
    , selectedLangs : Set.Set LanguageCode
    , transcriptDict : Dict.Dict LanguageCode Transcript
    }


type Msg
    = TalkResult (Result Http.Error Talk)
    | Transcript (Result ( LanguageCode, Http.Error ) ( LanguageCode, Transcript ))
    | Sidebar Sidebar.Msg
    | StoreLangs (List LanguageCode)


init : String -> ( Model, Cmd Msg )
init slug =
    ( { slug = slug
      , talk = Nothing
      , selectedLangs = Set.empty
      , transcriptDict = Dict.empty
      }
    , Cmd.batch
        [ LocalStorage.getLangs ()
        , getTalk slug
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreLangs langs ->
            ( { model | selectedLangs = Set.fromList langs }
            , Cmd.none
            )

        TalkResult (Ok talk) ->
            ( { model | talk = Just talk }
              -- selectedLangs is read from localStorage, StoreLangs comes before TalkResult
            , Cmd.batch <| List.map (getTranscript (Just talk)) (Set.toList model.selectedLangs)
            )

        TalkResult (Err _) ->
            ( model, Cmd.none )

        Transcript (Ok ( code, transcript )) ->
            ( { model
                | transcriptDict = Dict.insert code transcript model.transcriptDict
              }
            , Cmd.none
            )

        Transcript (Err ( code, _ )) ->
            ( { model | selectedLangs = Set.remove code model.selectedLangs }, Cmd.none )

        Sidebar (Sidebar.SelectLang lang) ->
            let
                ( newSet, needFetch ) =
                    if Set.member lang.code model.selectedLangs then
                        ( Set.remove lang.code model.selectedLangs, False )
                    else if Set.size model.selectedLangs < 2 then
                        ( Set.insert lang.code model.selectedLangs, True )
                    else
                        ( model.selectedLangs, False )

                newCmd =
                    if needFetch then
                        getTranscript model.talk lang.code
                    else
                        Cmd.none

                encodedLangs =
                    Set.toList newSet |> List.map Encode.string |> Encode.list |> Encode.encode 0
            in
                ( { model
                    | selectedLangs = newSet
                  }
                , Cmd.batch
                    [ newCmd
                    , LocalStorage.setLangs encodedLangs
                    ]
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    LocalStorage.onReceiveLangs
        (Decode.decodeString (Decode.list Decode.string)
            >> Result.toMaybe
            >> Maybe.withDefault []
            >> StoreLangs
        )


rowView : String -> String -> Html Msg
rowView p1 p2 =
    div [ class .row ]
        [ p [] [ text p1 ]
        , p [] [ text p2 ]
        ]


transcriptView : Model -> List (Html Msg)
transcriptView model =
    let
        transcripts =
            (Set.toList model.selectedLangs)
                |> List.filterMap
                    (\code -> Dict.get code model.transcriptDict)
                |> List.map
                    (\transcript -> String.split "\n" transcript)
    in
        case List.length transcripts of
            1 ->
                case List.head transcripts of
                    Just paragraphs ->
                        paragraphs |> List.map (\paragraph -> p [] [ text paragraph ])

                    Nothing ->
                        []

            2 ->
                case ( List.head transcripts, List.head <| List.drop 1 transcripts ) of
                    ( Just paragraphs1, Just paragraphs2 ) ->
                        List.map2 rowView paragraphs1 paragraphs2

                    _ ->
                        []

            _ ->
                []


view : Model -> Html Msg
view model =
    case model.talk of
        Just talk ->
            div []
                [ Header.view
                , div [ class .root ]
                    [ main_ [ class .main ]
                        [ TalkHeader.view talk
                        , article [] (transcriptView model)
                        ]
                    , aside []
                        [ Sidebar.view talk model.selectedLangs |> Html.map Sidebar
                        ]
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

                handleResult result =
                    case result of
                        Ok transcript ->
                            Transcript <| Ok ( code, transcript )

                        Err err ->
                            Transcript <| Err ( code, err )
            in
                Http.send handleResult (Http.getString url)

        Nothing ->
            Cmd.none
