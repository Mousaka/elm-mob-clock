port module Clock exposing (ClockState(..), Model, Msg(EnterPress, Finish, GotFocus, Start, StartNext, Tick, TimeToStart), displayTime, init, inputOrDisplayTime, subscriptions, update, view)

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
    | Finished


type Model
    = Idle IdleClock
    | Running RunningClock
    | Paused PausedClock


type alias Durationed a =
    { a | duration : Int, timeLeft : Int }


type alias IdleClock =
    Durationed
        { clockState : ClockState
        }


type alias RunningClock =
    Durationed
        { startTime : Time
        }


type alias PausedClock =
    Durationed
        { originalStart : Time
        , pauseStart : Time
        }


init : Model
init =
    Idle
        { duration = 60 * 10
        , clockState = Stopped
        , timeLeft = 60 * 10
        }



-- Messages


type Msg
    = Tick Time
    | Start Time
    | TimeToStart
    | TimeToPause
    | TimeToUnpause
    | StartNext
    | Reset
    | Pause Time
    | Unpause Time
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
        Tick time ->
            timeTick model time

        TimeToStart ->
            timeToStart model

        StartNext ->
            let
                ( m, c ) =
                    reset model

                ( m2, c2 ) =
                    timeToStart m
            in
            ( m2, Cmd.batch [ c, c2 ] )

        Start time ->
            case model of
                Idle m ->
                    ( Running { startTime = time, duration = m.duration, timeLeft = m.timeLeft }, Dom.focus "clock" |> Task.attempt FocusResult )

                Running _ ->
                    ( model, Cmd.none )

                Paused _ ->
                    ( model, Cmd.none )

        Reset ->
            reset model

        TimeToPause ->
            timeToPause model

        Pause time ->
            pause model time

        TimeToUnpause ->
            ( model, Cmd.batch [ Dom.focus "clock" |> Task.attempt FocusResult, Task.perform Unpause Time.now ] )

        Unpause time ->
            unpause model time

        Finish ->
            finish model

        SoundAlarm ->
            ( model, alarm () )

        SetTimer newTime ->
            setTimer model newTime

        EnterPress ->
            case model of
                Running _ ->
                    timeToPause model

                Idle m ->
                    case m.clockState of
                        Finished ->
                            timeToStart model

                        Stopped ->
                            timeToStart model

                Paused _ ->
                    timeToStart model

        FocusResult result ->
            ( model, Cmd.none )

        GotFocus ->
            ( model, Cmd.none )


finish model =
    case model of
        Running m ->
            ( Idle { duration = m.duration, clockState = Finished, timeLeft = m.timeLeft }, Cmd.none )

        _ ->
            ( model, Cmd.none )


unpause model time =
    case model of
        Paused m ->
            ( Running { duration = m.duration, startTime = m.originalStart + (m.pauseStart - m.originalStart), timeLeft = m.timeLeft }, Cmd.none )

        _ ->
            ( model, Cmd.none )


timeToPause model =
    case model of
        Idle _ ->
            ( model, Cmd.none )

        Paused _ ->
            ( model, Cmd.none )

        Running m ->
            ( model, Cmd.batch [ Dom.focus "clock" |> Task.attempt FocusResult, Task.perform Pause Time.now ] )


pause : Model -> Time -> ( Model, Cmd Msg )
pause model time =
    case model of
        Idle _ ->
            ( model, Cmd.none )

        Paused _ ->
            ( model, Cmd.none )

        Running m ->
            ( Paused { duration = m.duration, pauseStart = time, originalStart = m.startTime, timeLeft = m.timeLeft }, Cmd.none )


timeTick : Model -> Time -> ( Model, Cmd Msg )
timeTick model time =
    case model of
        Running m ->
            case (m.startTime + Basics.toFloat (m.duration * 1000)) > time of
                True ->
                    ( Running { m | timeLeft = round (((Basics.toFloat m.duration * 1000) - (time - m.startTime)) / 1000) }, Cmd.none )

                False ->
                    finish model

        Idle m ->
            ( model, Cmd.none )

        Paused m ->
            ( model, Cmd.none )


getDuration : Model -> Int
getDuration model =
    case model of
        Running m ->
            m.duration

        Idle m ->
            m.duration

        Paused m ->
            m.duration


reset : Model -> ( Model, Cmd Msg )
reset model =
    case model of
        Paused m ->
            ( Idle { clockState = Stopped, duration = m.duration, timeLeft = m.duration }, Dom.focus "clock" |> Task.attempt FocusResult )

        _ ->
            ( model, Cmd.none )


timeToStart : Model -> ( Model, Cmd Msg )
timeToStart model =
    case model of
        Running m ->
            ( model, Cmd.none )

        _ ->
            ( model, Task.perform Start Time.now )


setTimer model newTime =
    case toMinSec newTime of
        Just timeValue ->
            case model of
                Idle m ->
                    ( Idle { m | duration = timeValue }, Cmd.none )

                Paused m ->
                    ( Paused { m | duration = timeValue }, Cmd.none )

                Running _ ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Running _ ->
            Time.every second Tick

        Paused _ ->
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
            statusText model

        timeField =
            inputOrDisplayTime model <| displayTime <| getTime model

        startPauseResumeButton =
            startPauseResumeB model

        resetButton =
            resetB model
    in
    div [ Html.Attributes.id "clock" ]
        [ div [ flexMiddle ] [ model |> getTime |> clock ]
        , div [ flexMiddle ] [ text message ]
        , div [ flexMiddle ] [ timeField ]
        , div [ flexMiddle ] [ startPauseResumeButton, resetButton ]
        ]


getTime model =
    case model of
        Idle m ->
            m.timeLeft

        Paused m ->
            m.timeLeft

        Running m ->
            m.timeLeft


statusText : Model -> String
statusText model =
    case model of
        Idle m ->
            case m.clockState of
                Finished ->
                    "Time is up!"

                _ ->
                    ""

        _ ->
            ""


resetB : Model -> Html Msg
resetB model =
    case model of
        Idle model ->
            case model.clockState of
                Stopped ->
                    button [ onFocus GotFocus, onClick Reset, hidden True, myButton ] [ text "Reset" ]

                Finished ->
                    button [ onFocus GotFocus, Html.Attributes.id "startButton", onClick StartNext, myButton ] [ text "Start next" ]

        Paused _ ->
            button [ onFocus GotFocus, Html.Attributes.id "startButton", onClick Reset, myButton ] [ text "Reset" ]

        Running _ ->
            button [ onFocus GotFocus, onClick Reset, hidden True, myButton ] [ text "Reset" ]


startPauseResumeB : Model -> Html Msg
startPauseResumeB model =
    case model of
        Paused _ ->
            button [ onFocus GotFocus, Html.Attributes.id "startButton", onClick TimeToUnpause, myButton ]
                [ text "Resume" ]

        Running _ ->
            button [ onFocus GotFocus, Html.Attributes.id "startButton", onClick TimeToPause, myButton ]
                [ text "Pause" ]

        Idle model ->
            case model.clockState of
                Finished ->
                    button [ hidden True, myButton ] []

                Stopped ->
                    button [ onFocus GotFocus, Html.Attributes.id "startButton", onClick TimeToStart, myButton ]
                        [ text "Start" ]


inputOrDisplayTime : Model -> (String -> Html Msg)
inputOrDisplayTime model =
    case model of
        Paused _ ->
            displayTimer

        Running _ ->
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
