module Main exposing (main, view)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode
import Keyboard
import Task
import Time exposing (Posix, every)


type alias RunningTime =
    { started : Int, duration : Int, now : Int }


type alias StoppedTime =
    { duration : Int, now : Int }


toRunning : Int -> StoppedTime -> RunningTime
toRunning started stime =
    { started = started, duration = stime.duration, now = stime.now }


toStopped : RunningTime -> StoppedTime
toStopped rtime =
    { duration = rtime.duration, now = rtime.now }


type Model
    = Stopped StoppedTime
    | Running RunningTime


type Msg
    = Tick Posix
    | Start


emptyModel =
    { duration = 10 * 60 * 1000, now = 0 }


init _ =
    ( Stopped emptyModel, Cmd.none )


update msg model =
    case msg of
        Tick theTime ->
            case model of
                Running rModel ->
                    case timeIsUp rModel of
                        True ->
                            ( Stopped
                                ({ rModel | now = Time.posixToMillis theTime }
                                    |> toStopped
                                )
                            , Cmd.none
                            )

                        False ->
                            ( Running { rModel | now = Time.posixToMillis theTime }, Cmd.none )

                Stopped sModel ->
                    ( Stopped { sModel | now = Time.posixToMillis theTime }, Cmd.none )

        Start ->
            case model of
                Stopped sModel ->
                    ( Running (toRunning sModel.now sModel), Cmd.none )

                Running rModel ->
                    ( model, Cmd.none )



-- TIME Stuff


timeIsUp : RunningTime -> Bool
timeIsUp rtime =
    rtime.now >= rtime.duration + rtime.started


showTime timetrack =
    let
        timeLeft tt =
            tt.duration - (tt.now - tt.started)
    in
    timetrack |> timeLeft |> minutes |> seconds |> timeString


minutes : Int -> ( Int, Int )
minutes time =
    let
        min =
            time // (60 * 1000)
    in
    ( time - min * 60 * 1000, min )


seconds : ( Int, Int ) -> ( Int, Int )
seconds ( time, min ) =
    ( min, time // 1000 )


timeString : ( Int, Int ) -> String
timeString ( min, sec ) =
    let
        pad =
            Debug.toString >> String.padLeft 2 '0'
    in
    pad min ++ ":" ++ pad sec



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        Stopped sModel ->
            let
                body =
                    div [] [ text "Click to start session", button [ onClick Start ] [ text "Start" ] ]
            in
            { title = "Elm Mob clock", body = [ body ] }

        Running rModel ->
            let
                body =
                    div
                        []
                        [ text <| showTime rModel ]

                title =
                    showTime rModel
            in
            { title = title, body = [ body ] }



-- SUB


subscriptions model =
    Time.every 1000 Tick



-- MAIN


main : Program Json.Decode.Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
