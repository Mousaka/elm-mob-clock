import CountdownClock exposing (..)
import ParticipantQueue exposing (..)

import Html.App as Html
import Html exposing (Html, button, div, text)


main : Program Never
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
  , queue : ParticipantQueue.Model
  }

init : (Model, Cmd Msg)
init =
  ({countdownClock = CountdownClock.init
  , queue = ParticipantQueue.init}, Cmd.none)



-- UPDATE


type Msg
  = Clock CountdownClock.Msg
  | Queue ParticipantQueue.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
  case message of
    Clock msg ->
      case msg of
        Reset ->
          updateClockWithQueueRoation msg model
        _ ->
          let
            (newClockModel, clockCmds) =
              CountdownClock.update msg model.countdownClock
          in
            ( { model | countdownClock = newClockModel}, Cmd.map Clock clockCmds )

    Queue msg ->
      let
        (newQueueState, queueCmds) =
          ParticipantQueue.update msg model.queue
      in
        ( { model | queue = newQueueState}, Cmd.map Queue queueCmds )


updateClockWithQueueRoation : CountdownClock.Msg -> Model -> ( Model, Cmd Msg )
updateClockWithQueueRoation msg model =
  let
    (queueModel, queuecmd) =
      ParticipantQueue.update Next model.queue

    (newClockModel, clockCmds) =
      CountdownClock.update msg model.countdownClock

    mappedClockCmds = Cmd.map Clock clockCmds

    model' = {model | queue = queueModel, countdownClock = newClockModel}

  in
    (model', mappedClockCmds)

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
    , Html.map Queue (ParticipantQueue.view model.queue)
    ]
