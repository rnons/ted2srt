import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)


main =
  beginnerProgram { model = model, view = view, update = update }


type alias Model =
  { url : String
  }

model : Model
model =
  { url = ""
  }


type Msg = Input String | Submit

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input text -> { model | url = text}
    Submit -> { model | url = model.url ++ " SUBMITTED" }


view : Model -> Html Msg
view model =
  div []
    [ Html.form [ onSubmit Submit ]
        [ input [ type_ "text"
                , value model.url
                , placeholder "TED talk url or keywords"
                , onInput Input
                ] [],
          input [ type_ "submit"
                , hidden True
                ] []
        ]
    ]
