module TalkPage.Sidebar exposing (Msg(..), view)

import Set
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Models.Talk
    exposing
        ( Talk
        , Language
        , LanguageCode
        , TranscriptFormat(..)
        , talkDecoder
        , mkTranscriptDownloadUrl
        )
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
    let
        mkUrl bitrate =
            "https://download.ted.com/talks/"
                ++ talk.mediaSlug
                ++ "-"
                ++ bitrate
                ++ ".mp4"

        mkTitle resolution =
            "Resolution: " ++ resolution
    in
        div [ class .panel ]
            [ h4 [ class .panelTitle ] [ text "Download Video" ]
            , ul [ class .list ]
                (videoFormats
                    |> List.map
                        (\( fLabel, bitrate, resolution ) ->
                            li []
                                [ a
                                    [ href <| mkUrl bitrate
                                    , title <| mkTitle resolution
                                    , download True
                                    ]
                                    [ text fLabel ]
                                ]
                        )
                )
            ]


transcriptFormats =
    [ TXT, SRT, VTT ]


transcriptListView : Talk -> Set.Set LanguageCode -> Html Msg
transcriptListView talk selectedLangs =
    let
        mkUrl =
            mkTranscriptDownloadUrl talk selectedLangs
    in
        div [ class .panel ]
            [ h4 [ class .panelTitle ] [ text "Download Transcript" ]
            , ul [ class .list ]
                (transcriptFormats
                    |> List.map
                        (\format ->
                            li []
                                [ a [ href (mkUrl format) ] [ text <| toString format ]
                                ]
                        )
                )
            ]


languageListView : Talk -> Set.Set LanguageCode -> Html Msg
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
                                    (if Set.member language.code selectedLangs then
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


view : Talk -> Set.Set LanguageCode -> Html Msg
view talk selectedLangs =
    div []
        [ videoListView talk
        , transcriptListView talk selectedLangs
        , languageListView talk selectedLangs
        ]
