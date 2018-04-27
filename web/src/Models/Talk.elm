module Models.Talk exposing (..)

import Array exposing (fromList, get)
import Set
import Date
import String exposing (split)
import Regex exposing (HowMany(All), regex, replace)
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
    , published : Maybe Date.Date
    }


unescape : String -> String
unescape =
    replace All (regex "&amp;") (\_ -> "&")
        >> replace All (regex "&quot;") (\_ -> "\"")
        >> replace All (regex "&#39;") (\_ -> "'")
        >> replace All (regex "&lt;") (\_ -> "<")
        >> replace All (regex "&gt;") (\_ -> ">")


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
                    map unescape <|
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
            |> (custom <|
                    map unescape <|
                        field "description" string
               )
            |> required "mediaSlug" string
            |> (custom <| map (Result.toMaybe << Date.fromString) <| field "published" string)


formatString : TranscriptFormat -> String
formatString format =
    case format of
        TXT ->
            "txt"

        SRT ->
            "srt"

        VTT ->
            "vtt"


mkTranscriptUrl_ : Talk -> Set.Set LanguageCode -> TranscriptFormat -> Bool -> String
mkTranscriptUrl_ talk selectedLangs format isDownload =
    let
        query =
            case Set.size selectedLangs of
                0 ->
                    "lang=en"

                _ ->
                    String.join "&" <|
                        List.map (\code -> "lang=" ++ code) <|
                            Set.toList selectedLangs

        downloadSlug =
            if isDownload then
                "download/"
            else
                ""
    in
        "/api/talks/" ++ toString talk.id ++ "/transcripts/" ++ downloadSlug ++ formatString format ++ "?" ++ query


mkTranscriptUrl : Talk -> Set.Set LanguageCode -> TranscriptFormat -> String
mkTranscriptUrl talk selectedLangs format =
    mkTranscriptUrl_ talk selectedLangs format False


mkTranscriptDownloadUrl : Talk -> Set.Set LanguageCode -> TranscriptFormat -> String
mkTranscriptDownloadUrl talk selectedLangs format =
    mkTranscriptUrl_ talk selectedLangs format True
