module Styling exposing (myStyle, flexMiddle, myButton)

import Html exposing (Attribute)
import Html.Attributes exposing (style)

-- STYLE

myStyle : Html.Attribute msg
myStyle =
  style
    [ ("width", "170px")
    , ("height", "30px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("font-family", "Arial")
    , ("text-align", "center")
    ]

flexMiddle : Html.Attribute msg
flexMiddle =
  style
    [ ("display", "flex")
    , ("justify-content","center")]

myButton : Html.Attribute msg
myButton =
  style
    [ ("margin", "5px")
    , ("border", "1px solid #0B79CE")
    , ("background", "#0B79CE")
    , ("color", "#fff")
    , ("height", "30px")
    , ("width", "60px")
    , ("text-align", "center")
    ]
