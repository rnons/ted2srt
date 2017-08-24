module Models.Talk exposing (Talk, talkDecoder)

import Array exposing (fromList, get)
import String exposing (split)
import Json.Decode exposing (..)


type alias Talk =
    { slug : String
    , image : String
    , title : String
    , speaker : String
    }


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
        map4 Talk
            (field "slug" string)
            (map (String.join "&" << String.split "&amp;") <| field "image" string)
            (succeed <| Maybe.withDefault name title)
            (succeed <| Maybe.withDefault name speaker)
