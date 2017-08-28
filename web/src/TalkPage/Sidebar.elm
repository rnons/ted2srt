module TalkPage.Sidebar exposing (Msg(..), view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Models.Talk exposing (Talk, Language, talkDecoder)
import CssModules exposing (css)


{ class, classList } =
    css "./TalkPage/sidebar.css"
        { panel = ""
        , panelTitle = ""
        , list = ""
        , cover = ""
        , link = ""
        , linkActive = ""
        , description = ""
        }


videoFormats =
    [ ( "720p", "1500k", "1280x720" )
    , ( "480p", "950k", "854x480" )
    , ( "360p", "600k", "640x360" )
    , ( "288p", "320k", "512x288" )
    ]


type Msg
    = SelectLang Language


videoListView : Talk -> Html Msg
videoListView talk =
    div [ class .panel ]
        [ h4 [ class .panelTitle ] [ text "Download Video" ]
        , ul [ class .list ]
            (videoFormats
                |> List.map
                    (\( fLabel, bitrate, resolution ) ->
                        li []
                            [ a [ href "" ] [ text fLabel ]
                            ]
                    )
            )
        ]


transcriptFormats =
    [ "srt"
    , "txt"
    , "lrc"
    ]


transcriptListView : Html Msg
transcriptListView =
    div [ class .panel ]
        [ h4 [ class .panelTitle ] [ text "Download Transcript" ]
        , ul [ class .list ]
            (transcriptFormats
                |> List.map
                    (\format ->
                        li []
                            [ a [] [ text <| String.toUpper format ]
                            ]
                    )
            )
        ]


languageListView : Talk -> List Language -> Html Msg
languageListView talk selectedLangs =
    div [ class .panel ]
        [ h4 [ class .panelTitle ] [ text "Select Languages" ]
        , ul [ class .list ]
            (talk.languages
                |> List.map
                    (\language ->
                        li []
                            [ a
                                [ class
                                    (if List.member language selectedLangs then
                                        .linkActive
                                     else
                                        .link
                                    )
                                , onClick
                                    (SelectLang language)
                                ]
                                [ text language.endonym ]
                            ]
                    )
            )
        ]


view : Talk -> List Language -> Html Msg
view talk selectedLangs =
    div []
        [ videoListView talk
        , transcriptListView
        , languageListView talk selectedLangs
        ]
