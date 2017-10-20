module Utils exposing (getDateString, onPreventDefaultClick)

import Date
import Html exposing (Attribute)
import Html.Events exposing (onWithOptions, defaultOptions)
import Json.Decode exposing (Decoder)


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
            in
                month ++ " " ++ day ++ " " ++ year

        Nothing ->
            ""



-- https://github.com/elm-lang/navigation/issues/13#issuecomment-272996582


onPreventDefaultClick : msg -> Attribute msg
onPreventDefaultClick message =
    onWithOptions "click"
        { defaultOptions | preventDefault = True }
        (preventDefault2
            |> Json.Decode.andThen (maybePreventDefault message)
        )


preventDefault2 : Decoder Bool
preventDefault2 =
    Json.Decode.map2
        (invertedOr)
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "metaKey" Json.Decode.bool)


maybePreventDefault : msg -> Bool -> Decoder msg
maybePreventDefault msg preventDefault =
    case preventDefault of
        True ->
            Json.Decode.succeed msg

        False ->
            Json.Decode.fail "Normal link"


invertedOr : Bool -> Bool -> Bool
invertedOr x y =
    not (x || y)
