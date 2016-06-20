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
  { selected : Maybe Int
  , participants : List Participant
  , fieldText : String
  }


init : Model
init =
  { selected = Nothing , participants = [], fieldText = "" }



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
      case model.selected of
        Nothing ->
          ( {model | participants = participants', selected = Just 0}, Cmd.none)
        Just _ ->
          ( {model | participants = participants'}, Cmd.none)
    FieldText inputText ->
      ( {model | fieldText = inputText}, Cmd.none)
    Next ->
      case model.selected of
        Nothing ->
          ( {model |
            selected = (firstPersonIndex model.participants)}, Cmd.none)
        Just currentIndex ->
          ( {model |
            selected = nextPersonIndex currentIndex (List.length model.participants)}, Cmd.none)


firstPersonIndex : List Participant -> Maybe Int
firstPersonIndex participants =
  case List.isEmpty participants of
    True ->
      Nothing
    False ->
      Just 0


nextPersonIndex : Int -> Int -> Maybe Int
nextPersonIndex index listSize =
  let nextIndex = index + 1 in
  case nextIndex < listSize of
    True ->
      Just nextIndex
    False ->
      Just 0


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  div [] [
      displaySelected model.selected model.participants
    , displayParticipants model.participants
    , nameInput
    , addButton model.fieldText
    ]


displaySelected : Maybe Int -> List Participant -> Html Msg
displaySelected selected participants =
  case selected of
    Just index ->
     case get index participants of
       Just participant ->
         div [] [Html.text "Currently this participants turn: ", displayOneParticipant participant]
       Nothing ->
        div [] [Html.text "No one selected"]
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
