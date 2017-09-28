module TalkPage.Header exposing (view)

import Set
import Html exposing (..)
import Html.Attributes exposing (..)
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


view : Talk -> Set.Set LanguageCode -> Html msg
view talk selectedLangs =
    let
        videoUrl =
            "https://download.ted.com/talks/" ++ talk.mediaSlug ++ "-950k.mp4"

        vttUrl =
            getTranscriptUrl talk selectedLangs VTT
    in
        div []
            [ h3 [ class .title ]
                [ a [ href talk.slug ]
                    [ text (talk.speaker ++ ": " ++ talk.title) ]
                ]
            , Keyed.node "video"
                [ class .player, preload "none", controls True, hidden True ]
                [ ( "source", source [ src videoUrl, type_ "video/mp4" ] [] )
                , ( vttUrl, track [ kind "captions", src vttUrl, default True ] [] )
                ]
            , div [ class .info ]
                [ div
                    [ class .cover
                    , id "cover"
                    , style [ ( "backgroundImage", "url(" ++ talk.image ++ ")" ) ]
                    ]
                    [ span [ class .playButton ] []
                    ]
                , p [ class .description ]
                    [ text talk.description
                    , span [ class .date ] [ text ("Published: " ++ getDateString talk.publishedAt) ]
                    ]
                ]
            ]
