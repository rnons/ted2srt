module Main exposing (..)

import Html exposing (..)
import Navigation
import Route
import HomePage
import TalkPage


main =
    Navigation.program UrlChange
        { init = init, view = view, update = update, subscriptions = subscriptions }


type Page
    = Blank
    | Home HomePage.Model
    | Talk TalkPage.Model


type alias Model =
    { page : Page
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init loc =
    setRoute loc <| Model Blank


setRoute : Navigation.Location -> Model -> ( Model, Cmd Msg )
setRoute loc model =
    case Route.fromLocation loc of
        Just Route.Home ->
            let
                ( submodel, cmd ) =
                    HomePage.init
            in
                ( { model | page = Home submodel }, Cmd.map HomeMsg cmd )

        Just (Route.Talk slug) ->
            let
                ( submodel, cmd ) =
                    TalkPage.init slug
            in
                ( { model | page = Talk submodel }, Cmd.map TalkMsg cmd )

        _ ->
            ( { model | page = Blank }, Cmd.none )


type Msg
    = UrlChange Navigation.Location
    | HomeMsg HomePage.Msg
    | TalkMsg TalkPage.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | page = toModel newModel }, Cmd.map toMsg newCmd )
    in
        case ( msg, model.page ) of
            ( UrlChange loc, _ ) ->
                setRoute loc model

            ( HomeMsg msg, Home submodel ) ->
                toPage Home HomeMsg HomePage.update msg submodel

            ( TalkMsg msg, Talk submodel ) ->
                toPage Talk TalkMsg TalkPage.update msg submodel

            _ ->
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    case model.page of
        Home submodel ->
            HomePage.view submodel |> Html.map HomeMsg

        Talk submodel ->
            TalkPage.view submodel |> Html.map TalkMsg

        _ ->
            div [] [ text "loading" ]
