module TalkPage.Sidebar exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Models.Talk exposing (Talk, talkDecoder)
import CssModules exposing (css)


{ class, classList } =
    css "./TalkPage/sidebar.css"
        { panel = ""
        , panelTitle = ""
        , list = ""
        , cover = ""
        , description = ""
        }


videoFormats =
    [ ( "720p", "1500k", "1280x720" )
    , ( "480p", "950k", "854x480" )
    , ( "360p", "600k", "640x360" )
    , ( "288p", "320k", "512x288" )
    ]


videoListView : Talk -> Html msg
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


transcriptListView : Html msg
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


languageListView : Talk -> Html msg
languageListView talk =
    div [ class .panel ]
        [ h4 [ class .panelTitle ] [ text "Select Languages" ]
        , ul [ class .list ]
            (talk.languages
                |> List.map
                    (\language ->
                        li []
                            [ a [] [ text language.endonym ]
                            ]
                    )
            )
        ]


view : Talk -> Html msg
view talk =
    div []
        [ videoListView talk
        , transcriptListView
        , languageListView talk
        ]
