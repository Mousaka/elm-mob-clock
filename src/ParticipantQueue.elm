module ParticipantQueue exposing ( Model, Msg(Next), init, update, view, subscriptions )

import Html exposing (Html, div, button, input, Attribute)
import Html.App as Html
import Html exposing (Html, div, button, input, Attribute)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

main: Program Never
main =
  Html.program
    { init = init ! []
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type Participant = Name String

type alias Model =
  { participants : List Participant
  , fieldText : String
  }


init : Model
init =
  { participants = [], fieldText = "" }



-- UPDATE
type Msg
  = Add Participant
  | FieldText String
  | Next

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Add participant ->
      let participants' = List.append model.participants [participant] in
      ( {model | participants = participants'}, Cmd.none)

    FieldText inputText ->
      ( {model | fieldText = inputText}, Cmd.none)

    Next ->
      ( {model | participants = rotateQueue model.participants}, Cmd.none)




rotateQueue : List Participant -> List Participant
rotateQueue participants =
  let
    (head, tail) = (List.head participants, List.tail participants)
  in
  case (head, tail) of
    (Just h, Just t) ->
      List.append t [h]
    _ ->
      participants


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div [] [
      displaySelected model.participants
    , displayParticipants model.participants
    , nameInput
    , addButton model.fieldText
    ]


displaySelected : List Participant -> Html Msg
displaySelected participants =
  case get 0 participants of
    Just participant ->
      div [] [Html.text "Currently this participants turn: ", displayOneParticipant participant]
    Nothing ->
      div [] [Html.text "No one selected"]


get : Int -> List a -> Maybe a
get n list =
  List.head (List.drop n list)


displayParticipants : List Participant -> Html Msg
displayParticipants participants =
  div [style [("margin-top", "20px")]] (List.map displayOneParticipant participants)

displayOneParticipant : Participant -> Html Msg
displayOneParticipant participant =
  case participant of
    Name name ->
      div [] [Html.text name]


nameInput : Html Msg
nameInput =
  input [ placeholder "Add participant", onInput FieldText] []

addButton : String -> Html Msg
addButton fieldText =
  button [ onClick (Add (Name fieldText)) ] [ Html.text "Add" ]

nextButton : Html Msg
nextButton =
  button [ onClick Next ] [ Html.text "Next" ]
