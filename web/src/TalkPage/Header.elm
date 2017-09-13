module TalkPage.Header exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Models.Talk exposing (Talk, talkDecoder)
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


view : Talk -> Html msg
view talk =
    div []
        [ h3 [ class .title ]
            [ a [ href talk.slug ]
                [ text (talk.speaker ++ ": " ++ talk.title) ]
            ]
        , div [ class .info ]
            [ div
                [ class .cover
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
