port module CountdownClock exposing (..)
import Styling exposing (..)
import Util exposing (toMinSec)
import Html exposing (Html, div, button, input, Attribute)
import Html.Events exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Svg exposing (..)
import String exposing (toFloat, slice, right, length)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)

main: Program Never
main =
  Html.program
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
  {time : Int
  ,resetTime : Int
  ,clockState : ClockState
  }

init : Model
init =
  {time = 60*10
  ,resetTime = 60*10
  ,clockState = Stopped
  }


-- Messages


type Msg
  = Tick Time
  | Start
  | Reset
  | Pause
  | Unpause
  | Finish
  | SetTimer String


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tick _ ->
      case model.time < 1 of
        True ->
          update Finish model
        _ ->
          ({model | time = model.time - 1}, Cmd.none)
    Start ->
      ({model | clockState = Running}, Cmd.none)
    Reset ->
      ({model | clockState = Stopped, time = model.resetTime}, Cmd.none)
    Pause ->
      ({model | clockState = Paused}, Cmd.none)
    Unpause ->
      ({model | clockState = Running}, Cmd.none)
    Finish ->
      ({model | clockState = Finished}, alarm())
    SetTimer newTime ->
      case toMinSec newTime of
        Just timeValue ->
          ({model | resetTime = timeValue, time = timeValue}, Cmd.none)
        Nothing ->
          (model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.clockState of
    Running ->
      Time.every second Tick
    _ ->
       Sub.none

port alarm : () -> Cmd msg

-- VIEW

displayMin : Int -> Int
displayMin t = t // 60

displaySec : Int -> Int
displaySec t = t % 60

displayTime : Int -> String
displayTime time =
  let generalTimeDisplay = displayUnitsOfTime time in
  generalTimeDisplay displayMin ++ ":" ++ generalTimeDisplay displaySec


displayUnitsOfTime : Int -> (Int -> Int) -> String
displayUnitsOfTime time unitFunc =
  let unitsToDisplay = unitFunc time |> toString in
  case (length unitsToDisplay) of
    1 -> "0" ++ unitsToDisplay
    _ -> unitsToDisplay

view : Model -> Html Msg
view model =
  let
    message = statusText model.clockState
    timeField =  inputOrDisplayTime model.clockState <| displayTime model.time
    startPauseResumeButton = startPauseResumeB model.clockState
    resetButton = resetB model.clockState
  in
    div [] [
         div [flexMiddle] [clock model.time]
        ,div [] [text message]
        ,div [flexMiddle] [timeField]
        ,div [flexMiddle][startPauseResumeButton ,resetButton]
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
      button [ onClick Reset, myButton ] [ text "Reset" ]
    Finished ->
      button [ onClick Reset, myButton ] [ text "Reset" ]
    _ ->
      button [ onClick Reset, hidden True, myButton ] [ text "Reset" ]


startPauseResumeB : ClockState -> Html Msg
startPauseResumeB clockState =
  case clockState of
    Paused ->
      button [ onClick Unpause, myButton ] [ text "Resume" ]
    Running ->
      button [ onClick Pause, myButton ] [ text "Pause" ]
    Finished ->
      button [ hidden True, myButton ] []
    _ ->
      button [ onClick Start, myButton ] [ text "Start" ]


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
  input [ placeholder currentTime, onInput SetTimer, myStyle ] []


displayTimer : String -> Html Msg
displayTimer displayableTime =
  div [ myStyle ] [text displayableTime]


clock : Int -> Html Msg
clock time =
    let
      secondsAngle =
        angleHelper 60 time

      minutesAngle =
        angleHelper (60 * 60) time

      -- X Y coordinates for the handler tips
      secondHandlerTip =
        (50 + 40 * cos secondsAngle, 50 + 40 * sin secondsAngle)

      minutesHandlerTip =
        (50 + 38 * cos minutesAngle, 50 + 38 * sin minutesAngle)
    in
      svg [ viewBox "0 0 100 100", Svg.Attributes.width "300px" ]
          [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
          , clockHandle minutesHandlerTip "#000000"
          , clockHandle secondHandlerTip "#F0F8FF"
          ]

clockHandle : (Float, Float) -> String -> Svg Msg
clockHandle coords colour =
  let (x, y) = coords in
    line [ x1 "50", y1 "50", x2 (toString x), y2 (toString y), stroke colour ] []


angleHelper : Float -> Int -> Float
angleHelper speed seconds =
  pi * 2 * (Basics.toFloat seconds / speed) - pi / 2
