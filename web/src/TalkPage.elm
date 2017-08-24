module TalkPage exposing (..)

import Html exposing (..)


type alias Model =
    { slug : String }


type Msg
    = Loaded


init : String -> ( Model, Cmd Msg )
init slug =
    ( Model slug, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text <| "talk page, slug is " ++ model.slug ]
