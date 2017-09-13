module Models.Talk exposing (Talk, Language, LanguageCode, talkDecoder)

import Array exposing (fromList, get)
import Date
import String exposing (split)
import Json.Decode exposing (Decoder, field, int, list, string, andThen, map)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, custom)


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
            |> (custom <| map (Result.toMaybe << Date.fromString) <| field "publishedAt" string)
