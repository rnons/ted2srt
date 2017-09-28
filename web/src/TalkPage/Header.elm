module TalkPage.Header exposing (Msg(..), view)

import Set
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Models.Talk exposing (Talk, LanguageCode, TranscriptFormat(..), talkDecoder, getTranscriptUrl)
import CssModules exposing (css)
import Utils exposing (getDateString)


{ class, classList } =
    css "./TalkPage/index.css"
        { title = ""
        , player = ""
        , info = ""
        , cover = ""
        , description = ""
        , playButton = ""
        , date = ""
        }


type Msg
    = Play


viewTalkInfo : Talk -> Html Msg
viewTalkInfo talk =
    div [ class .info ]
        [ div
            [ class .cover
            , onClick Play
            , style [ ( "backgroundImage", "url(" ++ talk.image ++ ")" ) ]
            ]
            [ span [ class .playButton ] []
            ]
        , p [ class .description ]
            [ text talk.description
            , span [ class .date ] [ text ("Published: " ++ getDateString talk.publishedAt) ]
            ]
        ]


viewPlayer : Talk -> Set.Set LanguageCode -> Html Msg
viewPlayer talk selectedLangs =
    let
        videoUrl =
            "https://download.ted.com/talks/" ++ talk.mediaSlug ++ "-950k.mp4"

        vttUrl =
            getTranscriptUrl talk selectedLangs VTT
    in
        Keyed.node "video"
            [ class .player, preload "none", controls True, autoplay True ]
            [ ( "source", source [ src videoUrl, type_ "video/mp4" ] [] )
            , ( vttUrl, track [ kind "captions", src vttUrl, default True ] [] )
            ]


view : Talk -> Set.Set LanguageCode -> Bool -> Html Msg
view talk selectedLangs isPlaying =
    div []
        [ h3 [ class .title ]
            [ a [ href talk.slug ]
                [ text (talk.speaker ++ ": " ++ talk.title) ]
            ]
        , if isPlaying then
            viewPlayer talk selectedLangs
          else
            viewTalkInfo talk
        ]
