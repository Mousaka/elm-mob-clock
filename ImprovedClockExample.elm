import Html exposing (Html, div, button, input, Attribute)
import Html.Events exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Svg exposing (..)
import String exposing (toFloat)
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


type alias Model =
  {time : Float
  ,subscribingToTime : Bool
  ,finished : Bool
  }


init : Model
init =
  {time = 10, subscribingToTime = False, finished = False}


-- UPDATE


type Msg
  = Tick Time
  | Start
  | Stop
  | Reset
  | Finished
  | SetTimer String


update : Msg -> Model -> Model
update action model =
  case action of
    Tick _ ->
      case model.time of
        0 ->
          model |> update Finished |> update Stop
        _ -> {model | time = model.time - 1}
    Start ->
      {model | subscribingToTime = True}
    Stop ->
      {model | subscribingToTime = False}
    Finished ->
      {model | finished = True}
    Reset ->
      init
    SetTimer newTime ->
      case String.toFloat newTime of
        Ok timeValue ->
          {model | time = (timeValue)}
        Err message ->
          model

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
              ,input [ placeholder "Text to reverse", onInput SetTimer, myStyle ] []
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
      svg [ viewBox "0 0 100 100", Svg.Attributes.width "300px" ]
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

myStyle : Html.Attribute a
myStyle =
  Html.Attributes.style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
