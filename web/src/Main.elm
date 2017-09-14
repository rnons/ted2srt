module Main exposing (..)

import Regex exposing (..)
import Html exposing (..)
import Http
import Navigation
import Route
import Models.Talk exposing (Talk, talkDecoder)
import HomePage
import TalkPage
import SearchPage
import Components.Footer.Footer as Footer


main =
    Navigation.program UrlChange
        { init = init, view = view, update = update, subscriptions = subscriptions }


type Page
    = Blank
    | Home HomePage.Model
    | Talk TalkPage.Model
    | Search SearchPage.Model


type alias Model =
    { page : Page
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init loc =
    setRoute loc <| Model Blank


redirectToTalkPage : Model -> String -> ( Model, Cmd Msg )
redirectToTalkPage model slug =
    ( model, Navigation.newUrl ("/talks/" ++ slug) )


routeToTalkPage : Model -> String -> ( Model, Cmd Msg )
routeToTalkPage model slug =
    let
        ( submodel, cmd ) =
            TalkPage.init slug
    in
        ( { model | page = Talk submodel }
        , Cmd.map TalkMsg cmd
        )


routeToSearchPage : Model -> String -> ( Model, Cmd Msg )
routeToSearchPage model q =
    let
        ( submodel, cmd ) =
            SearchPage.init q
    in
        ( { model | page = Search submodel }, Cmd.map SearchMsg cmd )


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
            routeToTalkPage model slug

        Just (Route.Search q) ->
            case q of
                Just query ->
                    let
                        matches =
                            find (AtMost 1)
                                (regex "^https?://www.ted.com/talks/(\\w+)")
                                query
                    in
                        case (List.map .submatches matches) of
                            ((Just slug) :: _) :: _ ->
                                redirectToTalkPage model slug

                            _ ->
                                routeToSearchPage model query

                Nothing ->
                    ( { model | page = Blank }, Cmd.none )

        _ ->
            ( { model | page = Blank }, Cmd.none )


type Msg
    = UrlChange Navigation.Location
    | HomeMsg HomePage.Msg
    | TalkMsg TalkPage.Msg
    | SearchMsg SearchPage.Msg
    | FooterMsg Footer.Msg
    | RandomTalkResult (Result Http.Error Talk)


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

            ( SearchMsg msg, Search submodel ) ->
                toPage Search SearchMsg SearchPage.update msg submodel

            ( FooterMsg Footer.RandomTalk, _ ) ->
                ( model, getRandomTalk )

            ( RandomTalkResult (Ok talk), _ ) ->
                redirectToTalkPage model talk.slug

            _ ->
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        content =
            case model.page of
                Home submodel ->
                    HomePage.view submodel |> Html.map HomeMsg

                Talk submodel ->
                    TalkPage.view submodel |> Html.map TalkMsg

                Search submodel ->
                    SearchPage.view submodel |> Html.map SearchMsg

                _ ->
                    div [] [ text "loading" ]
    in
        div []
            [ content
            , Html.map FooterMsg Footer.view
            ]


getRandomTalk : Cmd Msg
getRandomTalk =
    let
        url =
            "/api/talks/random"
    in
        Http.send RandomTalkResult (Http.get url talkDecoder)
