module Models.Talk exposing (Talk, Language, LanguageCode, talkDecoder)

import Array exposing (fromList, get)
import String exposing (split)
import Json.Decode exposing (..)


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
    }


languageDecoder : Decoder Language
languageDecoder =
    map3 Language
        (field "languageCode" string)
        (field "endonym" string)
        (field "languageName" string)


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
        map8 Talk
            (field "id" int)
            (field "slug" string)
            (map (String.join "&" << String.split "&amp;") <| field "image" string)
            (succeed name)
            (succeed <| Maybe.withDefault name title)
            (succeed <| Maybe.withDefault name speaker)
            (field "languages" (list languageDecoder))
            (field "description" string)
