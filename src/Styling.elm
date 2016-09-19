module Styling exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (style)


-- STYLE


myStyle : Html.Attribute msg
myStyle =
    style <|
        List.append (textStyle 32)
            [ ( "width", "170px" )
            , ( "height", "30px" )
            , ( "padding", "10px 0" )
            ]


queueInput =
    style <|
        List.append (textStyle 20)
            [ ( "width", "170px" )
            , ( "height", "20px" )
            , ( "padding", "10px 0" )
            , ( "margin", "5px" )
            ]


textStyle size =
    [ ( "font-size", (toString size) ++ "px" )
    , ( "font-family", "Arial" )
    , ( "text-align", "center" )
    ]


mediumText : Html.Attribute msg
mediumText =
    style
        [ ( "font-size", "14pt" )
        , ( "font-family", "Arial" )
        , ( "text-align", "center" )
        ]


selectedStyle =
    style
        [ ( "font-size", "16pt" )
        , ( "font-weight", "bold" )
        ]


notSelectedStyle =
    style
        [ ( "font-size", "14pt" )
        , ( "color", "grey" )
        ]


flexMiddle : Html.Attribute msg
flexMiddle =
    style
        [ ( "display", "flex" )
        , ( "justify-content", "center" )
        , ( "flex-wrap", "wrap" )
        ]


myButton : Html.Attribute msg
myButton =
    style
        [ ( "margin", "5px" )
        , ( "border", "1px solid #0B79CE" )
        , ( "background", "#0B79CE" )
        , ( "color", "#fff" )
        , ( "height", "30px" )
        , ( "width", "60px" )
        , ( "text-align", "center" )
        ]


fromBottom : Html.Attribute msg
fromBottom =
    style
        [ ( "display", "flex" )
        , ( "justify-content", "flex-end" )
        , ( "flex-direction", "column" )
        , ( "align-items", "center" )
        ]
