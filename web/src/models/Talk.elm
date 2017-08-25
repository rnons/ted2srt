module Models.Talk exposing (Talk, Language, talkDecoder)

import Array exposing (fromList, get)
import String exposing (split)
import Json.Decode exposing (..)


type alias Language =
    { languageCode : String
    , endonym : String
    , languageName : String
    }


type alias Talk =
    { slug : String
    , image : String
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
        map6 Talk
            (field "slug" string)
            (map (String.join "&" << String.split "&amp;") <| field "image" string)
            (succeed <| Maybe.withDefault name title)
            (succeed <| Maybe.withDefault name speaker)
            (field "languages" (list languageDecoder))
            (field "description" string)
