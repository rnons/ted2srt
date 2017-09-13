module Utils exposing (getDateString)

import Date


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
