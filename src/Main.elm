module Main exposing (..)

import Clock exposing (..)
import ParticipantQueue exposing (..)
import Styling exposing (..)
import Html exposing (program)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import Keyboard


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { countdownClock : Clock.Model
    , queue : ParticipantQueue.Model
    , inFocus : Focused
    }


type Focused
    = TheClock
    | TheQueue


init : ( Model, Cmd Msg )
init =
    ( { countdownClock = Clock.init
      , queue = ParticipantQueue.init
      , inFocus = TheClock
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Clock Clock.Msg
    | Queue ParticipantQueue.Msg
    | KeyDown Int
    | Focus Focused


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Focus what ->
            ( { model | inFocus = what }, Cmd.none )

        KeyDown keyCode ->
            case keyCode of
                13 ->
                    case model.inFocus of
                        TheClock ->
                            let
                                ( newClockModel, clockCmds ) =
                                    Clock.update Clock.EnterPress model.countdownClock
                            in
                                ( { model | countdownClock = newClockModel }, Cmd.map Clock clockCmds )

                        TheQueue ->
                            let
                                ( newParticipantQueue, clockCmds ) =
                                    ParticipantQueue.update ParticipantQueue.EnterPress model.queue
                            in
                                ( { model | queue = newParticipantQueue }, Cmd.map Queue clockCmds )

                _ ->
                    ( model, Cmd.none )

        Clock msg ->
            case msg of
                Finish ->
                    updateClockWithQueueRoation msg model

                _ ->
                    let
                        ( newClockModel, clockCmds ) =
                            Clock.update msg model.countdownClock
                    in
                        ( { model | countdownClock = newClockModel }, Cmd.map Clock clockCmds )

        Queue msg ->
            let
                ( newQueueState, queueCmds ) =
                    ParticipantQueue.update msg model.queue
            in
                ( { model | queue = newQueueState }, Cmd.map Queue queueCmds )


updateClockWithQueueRoation : Clock.Msg -> Model -> ( Model, Cmd Msg )
updateClockWithQueueRoation msg model =
    let
        ( queueModel, queuecmd ) =
            ParticipantQueue.update Next model.queue

        ( newClockModel, clockCmds ) =
            Clock.update msg model.countdownClock

        mappedClockCmds =
            Cmd.map Clock clockCmds

        model_ =
            { model | queue = queueModel, countdownClock = newClockModel }
    in
        ( model_, mappedClockCmds )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Clock (Clock.subscriptions model.countdownClock)
        , Sub.map Queue (ParticipantQueue.subscriptions model.queue)
        , keyStrokesDispatcher
        ]


keyStrokesDispatcher : Sub Msg
keyStrokesDispatcher =
    Keyboard.downs KeyDown



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ flexMiddle ]
        [ div [ id "clock", onClick (Focus TheClock) ] [ Html.map Clock (Clock.view model.countdownClock) ]
        , div [ id "queue", onClick (Focus TheQueue) ] [ Html.map Queue (ParticipantQueue.view model.queue) ]
        ]
