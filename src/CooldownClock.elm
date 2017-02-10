module CooldownClock exposing (init, update, subscriptions, view, Model(..))

import Util exposing (msgAsCmd)
import Html exposing (program)
import Html.Attributes exposing (hidden)
import Html exposing (Html, div, button, input, Attribute)
import Clock exposing (ClockState(..), Msg(Start, StartNext, Finish, Tick), inputOrDisplayTime, displayTime)
import Time exposing (Time, second)


type Model
    = Deactivated
    | Active Clock.Model


init : Model
init =
    Active
        { time = 30
        , resetTime = 30
        , clockState = Stopped
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Active model_ ->
            case msg of
                Finish ->
                    ( Active { model_ | clockState = Running }, Cmd.none )

                StartNext ->
                    ( Active { model_ | clockState = Stopped, time = model_.resetTime }, Cmd.none )

                Start ->
                    ( Active { model_ | clockState = Stopped, time = model_.resetTime }, Cmd.none )

                Tick _ ->
                    case model_.time of
                        0 ->
                            ( Active { model_ | clockState = Stopped, time = model_.resetTime }, msgAsCmd StartNext )

                        _ ->
                            ( Active { model_ | time = model_.time - 1 }, Cmd.none )

                _ ->
                    ( Active model_, Cmd.none )

        Deactivated ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Active m ->
            let
                timeField =
                    inputOrDisplayTime m.clockState <| displayTime m.time
            in
                div [ hidden (m.clockState /= Running) ] [ timeField ]

        Deactivated ->
            div [] []


main : Program Never Model Msg
main =
    program
        { init = init ! []
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Active model_ ->
            case model_.clockState of
                Running ->
                    Time.every second Tick

                _ ->
                    Sub.none

        Deactivated ->
            Sub.none