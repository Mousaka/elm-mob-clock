import Html exposing (Html, div, button)
import Html.Events exposing (..)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Update.Extra.Infix exposing ((:>))

main: Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  {time : Float
  ,subscribingToTime : Bool
  ,finished : Bool
  }


init : (Model, Cmd Msg)
init =
  ({time = 10, subscribingToTime = False, finished = False}, Cmd.none)


-- UPDATE


type Msg
  = Tick Time
  | Start
  | Stop
  | Reset
  | Finished
  | AddTime


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Tick _ ->
      case model.time of
        0 ->
          (model, Cmd.none)
            :> update Finished
            :> update Stop
        _ -> ({model | time = model.time - 1}, Cmd.none)
    Start ->
      ({model | subscribingToTime = True}, Cmd.none)
    Stop ->
      ({model | subscribingToTime = False}, Cmd.none)
    Finished ->
      ({model | finished = True}, Cmd.none)
    Reset ->
      init
    AddTime ->
      ({model | time = model.time + 10}, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.subscribingToTime then
    Time.every second Tick
  else
    Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  let
    message = if model.finished then "Time is up!" else toString model.time
  in
    div [] [
        clock model.time
        , div [] [text message]
        , div [] [button [ onClick Start ] [ text "Start" ]
              ,button [ onClick Stop ] [ text "Stop" ]
              ,button [ onClick AddTime ] [ text "AddTime" ]
              ,button [ onClick Reset ] [ text "Reset timer" ]]
      ]

clock : Float -> Html Msg
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
      svg [ viewBox "0 0 100 100", width "300px" ]
          [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
          , clockHandle minutesHandlerTip "#000000"
          , clockHandle secondHandlerTip "#F0F8FF"
          ]

clockHandle : (Float, Float) -> String -> Svg Msg
clockHandle coords colour =
  let (x, y) = coords in
    line [ x1 "50", y1 "50", x2 (toString x), y2 (toString y), stroke colour ] []

angleHelper : Float -> Float -> Float
angleHelper speed seconds =
  pi * 2 * (seconds / speed) - pi / 2
