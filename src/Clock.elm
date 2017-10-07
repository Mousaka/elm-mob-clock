port module Clock exposing (ClockState(..), Model, Msg(EnterPress, Finish, GotFocus, Start, StartNext, Tick), displayTime, init, inputOrDisplayTime, subscriptions, update, view)

import Dom exposing (..)
import Html exposing (Attribute, Html, button, div, input, program)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (length, right, slice, toFloat)
import Styling exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task exposing (perform, succeed)
import Time exposing (Time, second)
import Util exposing (msgAsCmd, toMinSec)


main : Program Never Model Msg
main =
    program
        { init = init ! []
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type ClockState
    = Stopped
    | Running
    | Paused
    | Finished


type alias Model =
    { time : Int
    , resetTime : Int
    , clockState : ClockState
    }


init : Model
init =
    { time = 60 * 10
    , resetTime = 60 * 10
    , clockState = Stopped
    }



-- Messages


type Msg
    = Tick Time
    | Start
    | StartNext
    | Reset
    | Pause
    | Unpause
    | Finish
    | GotFocus
    | SoundAlarm
    | SetTimer String
    | EnterPress
    | FocusResult (Result Dom.Error ())



-- UPDATE


finishCmd : Task.Task Never Msg
finishCmd =
    succeed Finish


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case model.time of
                1 ->
                    update SoundAlarm { model | time = model.time - 1 }

                0 ->
                    ( model, Task.perform (\_ -> Finish) finishCmd )

                _ ->
                    ( { model | time = model.time - 1 }, Cmd.none )

        Start ->
            ( { model | clockState = Running }, Dom.focus "clock" |> Task.attempt FocusResult )

        StartNext ->
            ( { model | clockState = Stopped, time = model.resetTime }, msgAsCmd Start )

        Reset ->
            ( { model | clockState = Stopped, time = model.resetTime }, Dom.focus "clock" |> Task.attempt FocusResult )

        Pause ->
            ( { model | clockState = Paused }, Dom.focus "clock" |> Task.attempt FocusResult )

        Unpause ->
            ( { model | clockState = Running }, Dom.focus "clock" |> Task.attempt FocusResult )

        Finish ->
            ( { model | clockState = Finished }, Cmd.none )

        SoundAlarm ->
            ( model, alarm () )

        SetTimer newTime ->
            case toMinSec newTime of
                Just timeValue ->
                    ( { model | resetTime = timeValue, time = timeValue }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EnterPress ->
            case model.clockState of
                Running ->
                    update Pause model

                Finished ->
                    update StartNext model

                Stopped ->
                    update Start model

                Paused ->
                    update Start model

        FocusResult result ->
            ( model, Cmd.none )

        GotFocus ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.clockState of
        Running ->
            Time.every second Tick

        _ ->
            Sub.none



-- ports


port alarm : () -> Cmd msg



-- VIEW


displayMin : Int -> String
displayMin t =
    displayUnitsOfTime (t // 60)


displaySec : Int -> String
displaySec t =
    displayUnitsOfTime (t % 60)


displayTime : Int -> String
displayTime time =
    displayMin time ++ ":" ++ displaySec time


displayUnitsOfTime : Int -> String
displayUnitsOfTime time =
    let
        unitsToDisplay =
            toString time
    in
    case length unitsToDisplay of
        1 ->
            "0" ++ unitsToDisplay

        _ ->
            unitsToDisplay


view : Model -> Html Msg
view model =
    let
        message =
            statusText model.clockState

        timeField =
            inputOrDisplayTime model.clockState <| displayTime model.time

        startPauseResumeButton =
            startPauseResumeB model.clockState

        resetButton =
            resetB model.clockState
    in
    div [ Html.Attributes.id "clock" ]
        [ div [ flexMiddle ] [ clock model.time ]
        , div [ flexMiddle ] [ text message ]
        , div [ flexMiddle ] [ timeField ]
        , div [ flexMiddle ] [ startPauseResumeButton, resetButton ]
        ]


statusText : ClockState -> String
statusText clockState =
    case clockState of
        Finished ->
            "Time is up!"

        _ ->
            ""


resetB : ClockState -> Html Msg
resetB clockState =
    case clockState of
        Paused ->
            button [ onFocus GotFocus, Html.Attributes.id "startButton", onClick Reset, myButton ] [ text "Reset" ]

        Finished ->
            button [ onFocus GotFocus, Html.Attributes.id "startButton", onClick StartNext, myButton ] [ text "Start next" ]

        _ ->
            button [ onFocus GotFocus, onClick Reset, hidden True, myButton ] [ text "Reset" ]


startPauseResumeB : ClockState -> Html Msg
startPauseResumeB clockState =
    case clockState of
        Paused ->
            button [ onFocus GotFocus, Html.Attributes.id "startButton", onClick Unpause, myButton ]
                [ text "Resume" ]

        Running ->
            button [ onFocus GotFocus, Html.Attributes.id "startButton", onClick Pause, myButton ]
                [ text "Pause" ]

        Finished ->
            button [ hidden True, myButton ] []

        _ ->
            button [ onFocus GotFocus, Html.Attributes.id "startButton", onClick Start, myButton ]
                [ text "Start" ]


inputOrDisplayTime : ClockState -> (String -> Html Msg)
inputOrDisplayTime clockState =
    case clockState of
        Paused ->
            displayTimer

        Running ->
            displayTimer

        _ ->
            timerInput


timerInput : String -> Html Msg
timerInput currentTime =
    input [ onFocus GotFocus, placeholder currentTime, onInput SetTimer, myStyle ] []


displayTimer : String -> Html Msg
displayTimer displayableTime =
    div [ myStyle ] [ text displayableTime ]


clock : Int -> Html Msg
clock time =
    let
        secondsAngle =
            angleHelper 60 time

        minutesAngle =
            angleHelper (60 * 60) time

        -- X Y coordinates for the handler tips
        secondHandlerTip =
            ( 50 + 40 * cos secondsAngle, 50 + 40 * sin secondsAngle )

        minutesHandlerTip =
            ( 50 + 38 * cos minutesAngle, 50 + 38 * sin minutesAngle )
    in
    svg [ viewBox "0 0 100 100", Svg.Attributes.width "250px" ]
        [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
        , clockHandle minutesHandlerTip "#000000"
        , clockHandle secondHandlerTip "#F0F8FF"
        ]


clockHandle : ( Float, Float ) -> String -> Svg Msg
clockHandle coords colour =
    let
        ( x, y ) =
            coords
    in
    line [ x1 "50", y1 "50", x2 (toString x), y2 (toString y), stroke colour ] []


angleHelper : Float -> Int -> Float
angleHelper speed seconds =
    pi * 2 * (Basics.toFloat seconds / speed) - pi / 2
