module Models.Talk exposing (..)

import Array exposing (fromList, get)
import Set
import Date
import String exposing (split)
import Json.Decode exposing (Decoder, field, int, list, string, andThen, map)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, custom)


type TranscriptFormat
    = TXT
    | SRT
    | VTT


type alias LanguageCode =
    String


type alias Language =
    { code : LanguageCode
    , endonym : String
    , name : String
    }


type alias Talk =
    { id : Int
    , slug : String
    , image : String
    , name : String
    , title : String
    , speaker : String
    , languages : List Language
    , description : String
    , mediaSlug : String
    , publishedAt : Maybe Date.Date
    }


languageDecoder : Decoder Language
languageDecoder =
    decode Language
        |> required "languageCode" string
        |> required "endonym" string
        |> required "languageName" string


talkDecoder : Decoder Talk
talkDecoder =
    field "name" string |> andThen talkNameDecoder


talkNameDecoder : String -> Decoder Talk
talkNameDecoder name =
    let
        array =
            fromList <| split ":" name

        speaker =
            get 0 array

        title =
            get 1 array
    in
        decode Talk
            |> required "id" int
            |> required "slug" string
            |> (custom <|
                    map (String.join "&" << String.split "&amp;") <|
                        field "image" string
               )
            |> hardcoded name
            |> (hardcoded <|
                    Maybe.withDefault name title
               )
            |> (hardcoded <|
                    Maybe.withDefault name speaker
               )
            |> required "languages" (list languageDecoder)
            |> required "description" string
            |> required "mediaSlug" string
            |> (custom <| map (Result.toMaybe << Date.fromString) <| field "publishedAt" string)


formatString : TranscriptFormat -> String
formatString format =
    case format of
        TXT ->
            "txt"

        SRT ->
            "srt"

        VTT ->
            "vtt"


getTranscriptUrl : Talk -> Set.Set LanguageCode -> TranscriptFormat -> String
getTranscriptUrl talk selectedLangs format =
    let
        query =
            case Set.size selectedLangs of
                0 ->
                    "lang=en"

                _ ->
                    String.join "&" <|
                        List.map (\code -> "lang=" ++ code) <|
                            Set.toList selectedLangs
    in
        "/api/talks/" ++ toString talk.id ++ "/transcripts/" ++ formatString format ++ "?" ++ query
