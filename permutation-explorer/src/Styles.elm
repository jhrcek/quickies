module Styles exposing (buttonAttrs)

import Html exposing (Attribute)
import Html.Attributes exposing (style)


buttonAttrs : List (Attribute msg)
buttonAttrs =
    [ style "padding" "8px 12px"
    , style "font-size" "14px"
    , style "border" "1px solid #ccc"
    , style "border-radius" "4px"
    , style "background" "#fff"
    , style "color" "#000"
    , style "cursor" "pointer"
    ]
