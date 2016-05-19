import Html exposing (Html, div, button)
import Html.Events exposing (..)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


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
  ({time = 5, subscribingToTime = False, finished = False}, Cmd.none)


-- UPDATE


type Msg
  = Tick Time
  | Start
  | Stop
  | Reset
  | Finished


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Tick _ ->
      case model.time of
        0 -> update Finished model
        _ -> ({model | time = model.time - 1}, Cmd.none)
    Start ->
      ({model | subscribingToTime = True}, Cmd.none)
    Stop ->
      ({model | subscribingToTime = False}, Cmd.none)
    Finished ->
      ({model | finished = True}, Cmd.none)
    Reset ->
      init

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
    secondsAngle =
      angleHelper 60 model.time

    minutesAngle =
      angleHelper (60 * 60) model.time

    secondsHandX =
      toString (50 + 40 * cos secondsAngle)

    secondsHandY =
      toString (50 + 40 * sin secondsAngle)

    minutesHandX =
      toString (50 + 30 * cos minutesAngle)

    minutesHandY =
      toString (50 + 30 * sin minutesAngle)
  in
    div [] [
    svg [ viewBox "0 0 100 100", width "300px" ]
        [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
        , line [ x1 "50", y1 "50", x2 minutesHandX, y2 minutesHandY, stroke "#000000" ] []
        , line [ x1 "50", y1 "50", x2 secondsHandX, y2 secondsHandY, stroke "#F0F8FF" ] []
        ],
        div [] [text (toString model.time)]
        ,div [] [button [ onClick Start ] [ text "Start" ]
              ,button [ onClick Stop ] [ text "Stop" ]
              ,button [ onClick Reset ] [ text "Reset timer" ]]
      ]

angleHelper : Float -> Float -> Float
angleHelper speed seconds =
  pi * 2 * (seconds / speed) - pi / 2
