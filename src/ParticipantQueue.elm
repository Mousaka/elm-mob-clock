module ParticipantQueue exposing (Model, Msg(Next, EnterPress, GotFocus), init, update, view, subscriptions)

import Styling exposing (..)
import Html exposing (Html, div, button, input, Attribute)
import Html exposing (program)
import Html exposing (Html, div, button, input, Attribute)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Svg exposing (..)
import String exposing (..)
import Svg.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init ! []
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { participants : List String
    , fieldText : String
    }


init : Model
init =
    { participants = [], fieldText = "" }



-- UPDATE


type Msg
    = Add (Maybe String)
    | FieldText String
    | Next
    | EnterPress
    | GotFocus


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnterPress ->
            update (Add (validInput model.fieldText)) model

        Add participant ->
            case participant of
                Just name ->
                    let
                        participants_ =
                            List.append model.participants [ name ]
                    in
                        ( { model | participants = participants_, fieldText = "" }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        FieldText inputText ->
            ( { model | fieldText = inputText }, Cmd.none )

        Next ->
            ( { model | participants = rotateQueue model.participants }, Cmd.none )

        GotFocus ->
            ( model, Cmd.none )


rotateQueue : List String -> List String
rotateQueue participants =
    let
        ( head, tail ) =
            ( List.head participants, List.tail participants )
    in
        case ( head, tail ) of
            ( Just h, Just t ) ->
                List.append t [ h ]

            _ ->
                participants



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ mediumText, fromBottom ]
        [ displayParticipants model.participants
        , rotate
        , nameInput model.fieldText
        , add model.fieldText
        ]


get : Int -> List a -> Maybe a
get n list =
    List.head (List.drop n list)


displayParticipants : List String -> Html Msg
displayParticipants participants =
    case participants of
        h :: t ->
            div [ Html.Attributes.style [ ( "margin-top", "20px" ) ] ] (displaySelected h :: List.map displayOneParticipant t)

        [] ->
            div [ Html.Attributes.style [ ( "margin-top", "20px" ) ] ] []


displaySelected : String -> Html Msg
displaySelected participant =
    div [ selectedStyle ] [ Html.text participant ]


displayOneParticipant : String -> Html Msg
displayOneParticipant participant =
    div [ notSelectedStyle ] [ Html.text participant ]


nameInput : String -> Html Msg
nameInput fieldText =
    div []
        [ input [ onFocus GotFocus, minlength 1, queueInput, placeholder "Add participant", onInput FieldText, value fieldText ] []
        ]


nextButton : Html Msg
nextButton =
    button [ onClick Next ] [ Html.text "Next" ]


add : String -> Html Msg
add fieldText =
    div []
        [ svg [ viewBox "0 0 100 100", Svg.Attributes.width "30px", Svg.Attributes.height "30px", onClick (Add (validInput fieldText)) ]
            [ rect [ x "40", y "0", Svg.Attributes.width "20", Svg.Attributes.height "100", fill "#0B79CE" ] []
            , rect [ x "0", y "40", Svg.Attributes.width "100", Svg.Attributes.height "20", fill "#0B79CE" ] []
            ]
        ]


rotate : Html Msg
rotate =
    div []
        [ svg [ viewBox "0 0 90 90", Svg.Attributes.width "35px", Svg.Attributes.height "35px", onClick Next ]
            [ circle [ cx "50", cy "50", r "30", stroke "#0B79CE", strokeWidth "18", fill "none" ] []
            , rect [ x "0", y "40", Svg.Attributes.width "100", Svg.Attributes.height "20", fill "white" ] []
            , rect [ x "13", y "26", Svg.Attributes.width "20", Svg.Attributes.height "20", fill "#0B79CE" ] []
            , rect [ x "68", y "55", Svg.Attributes.width "20", Svg.Attributes.height "20", fill "#0B79CE" ] []
            ]
        ]


validInput : String -> Maybe String
validInput text =
    if String.length text > 0 then
        Just text
    else
        Nothing
