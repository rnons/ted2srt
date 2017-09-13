module SearchPage exposing (Msg, Model, init, view, update)

import Html exposing (..)
import Html.Attributes exposing (href, style)
import Http
import Json.Decode as Decode
import Models.Talk exposing (Talk, talkDecoder)
import CssModules exposing (css)
import Components.Header.Header as Header
import Utils exposing (getDateString)


{ class, classList } =
    css "./SearchPage/index.css"
        { root = ""
        , item = ""
        , info = ""
        , cover = ""
        , description = ""
        , date = ""
        }


type Msg
    = SearchResult (Result Http.Error (List Talk))


type alias Model =
    { q : String
    , talks : List Talk
    }


init : String -> ( Model, Cmd Msg )
init q =
    ( { q = q
      , talks = []
      }
    , searchTalk q
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchResult (Ok talks) ->
            ( { model | talks = talks }, Cmd.none )

        SearchResult (Err _) ->
            ( model, Cmd.none )


talkView : Talk -> Html msg
talkView talk =
    let
        talkUrl =
            "/talks/" ++ talk.slug
    in
        div [ class .item ]
            [ h3 []
                [ a [ href talkUrl ] [ text talk.name ]
                ]
            , div [ class .info ]
                [ a
                    [ class .cover
                    , style [ ( "backgroundImage", "url(" ++ talk.image ++ ")" ) ]
                    , href talkUrl
                    ]
                    []
                , div []
                    [ p []
                        [ text talk.description
                        , span [ class .date ] [ text ("Published: " ++ getDateString talk.publishedAt) ]
                        ]
                    ]
                ]
            ]


view : Model -> Html msg
view model =
    div []
        [ Header.view
        , div [ class .root ]
            (List.map talkView model.talks)
        ]


searchTalk : String -> Cmd Msg
searchTalk q =
    let
        url =
            "/api/search?q=" ++ q
    in
        Http.send SearchResult (Http.get url <| Decode.list talkDecoder)
