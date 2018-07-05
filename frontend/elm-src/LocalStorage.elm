port module LocalStorage exposing (..)


port getLangs : () -> Cmd msg


port setLangs : String -> Cmd msg


port onReceiveLangs : (String -> msg) -> Sub msg
