module KaTeX exposing (display, inline)

import Html exposing (Html, node)
import Html.Attributes exposing (attribute)


tex : Bool -> String -> Html msg
tex displayMode formula =
    node "math-tex"
        [ attribute "formula" formula
        , attribute "display-mode"
            (if displayMode then
                "true"

             else
                "false"
            )
        ]
        []


inline : String -> Html msg
inline =
    tex False


display : String -> Html msg
display =
    tex True
