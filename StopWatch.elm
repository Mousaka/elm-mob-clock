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
      , update = (\a m-> update a m ! [])
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
  ,finished : Bool
  }

init : Model
init =
  {time = 60*10, resetTime = 60*10, clockState = Stopped, finished = False}


-- UTIL

type ParsedTime
  = Unparsable
  | ParsedValue Int


toMinSec : String -> ParsedTime
toMinSec textTime =
  let
    mins = slice -4 -2 textTime
    sec = right 2 textTime
    minSec = (toParsedTime mins, toParsedTime sec)
  in
  case minSec of
    (Unparsable, ParsedValue seconds) ->
      ParsedValue seconds
    (ParsedValue minutes, ParsedValue seconds) ->
      ParsedValue (60 * minutes + seconds)
    _ -> Unparsable


toParsedTime : String -> ParsedTime
toParsedTime timeToParse =
  case String.toInt timeToParse of
    Ok timeValue ->
      ParsedValue timeValue
    Err _ ->
      Unparsable


-- UPDATE


type Msg
  = Tick Time
  | Start
  | Reset
  | Pause
  | Unpause
  | Finish
  | SetTimer String


update : Msg -> Model -> Model
update action model =
  case action of
    Tick _ ->
      case model.time of
        0 ->
          model |> update Finish
        _ -> {model | time = model.time - 1}
    Start ->
      {model | clockState = Running, finished = False}
    Reset ->
      {model | clockState = Stopped, time = model.resetTime}
    Pause ->
      {model | clockState = Paused}
    Unpause ->
      {model | clockState = Running}
    Finish ->
      {model | clockState = Finished}
    SetTimer newTime ->
      case toMinSec newTime of
        ParsedValue timeValue ->
          {model | resetTime = timeValue, time = timeValue}
        Unparsable ->
          model

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.clockState of
    Running ->
      Time.every second Tick
    _ ->
       Sub.none



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
    div [Html.Attributes.style [("text-align", "center")]] [
        clock model.time
        , div [] [text message]
        , div [] [
              timeField
              ,startPauseResumeButton
              ,resetButton
              ]
      ]

resetB : ClockState -> Html Msg
resetB clockState =
  case clockState of
    Paused ->
      button [ onClick Reset ] [ text "Reset" ]
    Finished ->
      button [ onClick Reset ] [ text "Reset" ]
    _ ->
      button [ onClick Reset, hidden True ] [ text "Reset" ]

statusText : ClockState -> String
statusText clockState =
  case clockState of
    Finished ->
      "Time is up!"
    _ ->
      ""

startPauseResumeB : ClockState -> Html Msg
startPauseResumeB clockState =
  case clockState of
    Paused ->
      button [ onClick Unpause ] [ text "Resume" ]
    Running ->
      button [ onClick Pause ] [ text "Pause" ]
    Finished ->
      button [ hidden True ] []
    _ ->
      button [ onClick Start ] [ text "Start" ]

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
  div [myStyle ] [text displayableTime]

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
        (50 + 30 * cos minutesAngle, 50 + 30 * sin minutesAngle)
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

myStyle : Html.Attribute a
myStyle =
  Html.Attributes.style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
