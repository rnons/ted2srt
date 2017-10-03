module Main exposing (main)

import Regex exposing (..)
import Html exposing (..)
import Http
import Navigation
import Route
import Task
import Models.Talk exposing (Talk, talkDecoder)
import HomePage
import TalkPage
import SearchPage
import NotFoundPage
import Components.Footer.Footer as Footer
import Components.Loading.Loading as Loading


main =
    Navigation.program UrlChange
        { init = init, view = view, update = update, subscriptions = subscriptions }


type Page
    = Blank
    | NotFound
    | Home HomePage.Model
    | Talk TalkPage.Model
    | Search SearchPage.Model


type PageStatus
    = RedirectFrom Page
    | Loaded Page


type alias Model =
    { pageStatus : PageStatus
    }


getPage : PageStatus -> Page
getPage pageStatus =
    case pageStatus of
        RedirectFrom page ->
            page

        Loaded page ->
            page


init : Navigation.Location -> ( Model, Cmd Msg )
init loc =
    setRoute loc <| Model (Loaded Blank)


redirectToTalkPage : Model -> String -> ( Model, Cmd Msg )
redirectToTalkPage model slug =
    ( model, Navigation.newUrl ("/talks/" ++ slug) )


setRoute : Navigation.Location -> Model -> ( Model, Cmd Msg )
setRoute loc model =
    let
        redirectTo toMsg req =
            ( { model | pageStatus = RedirectFrom (getPage model.pageStatus) }
            , Task.attempt toMsg req
            )
    in
        case Route.fromLocation loc of
            Just Route.Home ->
                redirectTo HomeLoaded (HomePage.init)

            Just (Route.Talk slug) ->
                redirectTo TalkLoaded (TalkPage.init slug)

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
                                    redirectTo SearchLoaded (SearchPage.init query)

                    Nothing ->
                        ( { model | pageStatus = Loaded NotFound }, Cmd.none )

            _ ->
                ( { model | pageStatus = Loaded NotFound }, Cmd.none )


type Msg
    = UrlChange Navigation.Location
    | HomeLoaded (Result Http.Error HomePage.Model)
    | TalkLoaded (Result Http.Error TalkPage.Model)
    | SearchLoaded (Result Http.Error SearchPage.Model)
    | TalkMsg TalkPage.Msg
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
                ( { model | pageStatus = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )
    in
        case ( msg, model.pageStatus ) of
            ( UrlChange loc, _ ) ->
                setRoute loc model

            ( TalkMsg msg, Loaded (Talk submodel) ) ->
                toPage Talk TalkMsg TalkPage.update msg submodel

            ( HomeLoaded (Ok submodel), _ ) ->
                ( { model | pageStatus = Loaded (Home submodel) }, Cmd.none )

            ( TalkLoaded (Ok submodel), _ ) ->
                ( { model | pageStatus = Loaded (Talk submodel) }
                , Cmd.map TalkMsg TalkPage.onLoad
                )

            ( SearchLoaded (Ok submodel), _ ) ->
                ( { model | pageStatus = Loaded (Search submodel) }, Cmd.none )

            ( FooterMsg Footer.RandomTalk, _ ) ->
                ( model, getRandomTalk )

            ( RandomTalkResult (Ok talk), _ ) ->
                redirectToTalkPage model talk.slug

            _ ->
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.pageStatus of
        Loaded (Talk submodel) ->
            TalkPage.subscriptions submodel |> Sub.map TalkMsg

        _ ->
            Sub.none


view : Model -> Html Msg
view model =
    let
        content =
            case model.pageStatus of
                RedirectFrom _ ->
                    Loading.view

                Loaded Blank ->
                    text ""

                Loaded NotFound ->
                    NotFoundPage.view

                Loaded (Home submodel) ->
                    HomePage.view submodel

                Loaded (Talk submodel) ->
                    TalkPage.view submodel |> Html.map TalkMsg

                Loaded (Search submodel) ->
                    SearchPage.view submodel
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
