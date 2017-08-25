module TalkPage.Header exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Models.Talk exposing (Talk, talkDecoder)
import CssModules exposing (css)


{ class, classList } =
    css "./TalkPage/index.css"
        { title = ""
        , player = ""
        , info = ""
        , cover = ""
        , description = ""
        }


view : Talk -> Html msg
view talk =
    div []
        [ h3 [ class .title ]
            [ a [ href talk.slug ]
                [ text (talk.speaker ++ ": " ++ talk.title) ]
            ]
        , div [ class .info ]
            [ div [ class .cover ] []
            , div [ class .description ] [ text talk.description ]
            ]
        ]
