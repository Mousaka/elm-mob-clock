module Main exposing (..)

import Clock exposing (..)
import CooldownClock exposing (..)
import ParticipantQueue exposing (..)
import Styling exposing (..)
import Html exposing (program)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick, onFocus)
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
    , cooldownClock : CooldownClock.Model
    , queue : ParticipantQueue.Model
    , inFocus : Focused
    }


type Focused
    = TheClock
    | TheQueue


init : ( Model, Cmd Msg )
init =
    ( { countdownClock = Clock.init
      , cooldownClock = CooldownClock.init
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
                StartNext ->
                    updateClockWithQueueRoation msg model

                Clock.GotFocus ->
                    ( { model | inFocus = TheClock }, Cmd.none )

                Tick _ ->
                    whoToTick msg model

                _ ->
                    let
                        ( newClockModel, clockCmds ) =
                            Clock.update msg model.countdownClock

                        ( newCooldownClockModel, cooldownClockCmd ) =
                            CooldownClock.update msg model.cooldownClock
                    in
                        ( { model
                            | countdownClock = newClockModel
                            , cooldownClock = newCooldownClockModel
                          }
                        , Cmd.map Clock clockCmds
                        )

        Queue msg ->
            let
                ( newQueueState, queueCmds ) =
                    ParticipantQueue.update msg model.queue

                ( newModel, cmd ) =
                    ( { model | queue = newQueueState }, Cmd.map Queue queueCmds )
            in
                case msg of
                    ParticipantQueue.GotFocus ->
                        ( { newModel | inFocus = TheQueue }, cmd )

                    _ ->
                        ( newModel, cmd )


whoToTick : Clock.Msg -> Model -> ( Model, Cmd Msg )
whoToTick msg model =
    if model.countdownClock.clockState == Running then
        let
            ( newState, newCmd ) =
                Clock.update msg model.countdownClock
        in
            ( { model | countdownClock = newState }, Cmd.map Clock newCmd )
    else
        case model.cooldownClock of
            Active m ->
                if m.clockState == Running then
                    let
                        ( newState, newCmd ) =
                            CooldownClock.update msg model.cooldownClock
                    in
                        ( { model | cooldownClock = newState }, Cmd.map Clock newCmd )
                else
                    ( model, Cmd.none )

            Deactivated ->
                ( model, Cmd.none )


updateClockWithQueueRoation : Clock.Msg -> Model -> ( Model, Cmd Msg )
updateClockWithQueueRoation msg model =
    let
        ( queueModel, queuecmd ) =
            ParticipantQueue.update Next model.queue

        ( newClockModel, clockCmd ) =
            Clock.update msg model.countdownClock

        ( newCooldownClockModel, cooldownClockCmd ) =
            CooldownClock.update msg model.cooldownClock

        mappedQueueCmd =
            Cmd.map Queue queuecmd

        mappedClockCmd =
            Cmd.map Clock clockCmd

        mappedCooldownClockCmd =
            Cmd.map Clock cooldownClockCmd

        batchedCmds =
            Cmd.batch [ mappedQueueCmd, mappedClockCmd, mappedCooldownClockCmd ]

        model_ =
            { model
                | queue = queueModel
                , countdownClock = newClockModel
                , cooldownClock = newCooldownClockModel
            }
    in
        ( model_, batchedCmds )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Clock (Clock.subscriptions model.countdownClock)
        , Sub.map Queue (ParticipantQueue.subscriptions model.queue)
        , Sub.map Clock (CooldownClock.subscriptions model.cooldownClock)
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
        [ div [] [ Html.map Clock (CooldownClock.view model.cooldownClock) ]
        , div [ onClick (Focus TheClock) ] [ Html.map Clock (Clock.view model.countdownClock) ]
        , div [ id "queue", onClick (Focus TheQueue), onFocus (Focus TheQueue) ] [ Html.map Queue (ParticipantQueue.view model.queue) ]
        ]
