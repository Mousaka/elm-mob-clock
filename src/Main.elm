import CountdownClock

import Html.App as Html
import Html exposing (Html, button, div, text)



main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { countdownClock : CountdownClock.Model
  }

init : (Model, Cmd Msg)
init =
  ({ countdownClock = CountdownClock.init}, Cmd.none)



-- UPDATE


type Msg
  = Clock CountdownClock.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
  case message of
    Clock msg ->
      let
        (newClockState, clockCmds) =
          CountdownClock.update msg model.countdownClock
      in
      ({ model | countdownClock = newClockState }
      , Cmd.map Clock clockCmds)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [ Sub.map Clock (CountdownClock.subscriptions model.countdownClock) ]

-- VIEW


view : Model -> Html Msg
view model =
  div
    []
    [ Html.map Clock (CountdownClock.view model.countdownClock)
    ]
