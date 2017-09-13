module TalkPage.Header exposing (view)

import Date
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
        , playButton = ""
        , date = ""
        }


getDateString : Maybe Date.Date -> String
getDateString mDate =
    case mDate of
        Just date ->
            let
                year =
                    toString <| Date.year date

                month =
                    toString <| Date.month date

                day =
                    String.padLeft 2 '0' <| toString <| Date.day date

                dateString =
                    month ++ " " ++ day ++ " " ++ year
            in
                "Published: " ++ dateString

        Nothing ->
            ""


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
                , span [ class .date ] [ text (getDateString talk.publishedAt) ]
                ]
            ]
        ]
