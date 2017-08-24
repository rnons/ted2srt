module TalkPage exposing (..)

import Html exposing (..)


type alias Model =
    { id : Int }


type Msg
    = Loaded


init : ( Model, Cmd Msg )
init =
    ( Model 0, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text "talk page" ]
