module KaTeX exposing (inline)

import Html exposing (Html, node)
import Html.Attributes exposing (attribute)


tex : Bool -> String -> Html msg
tex display formula =
    node "math-tex"
        [ attribute "formula" formula
        , attribute "display-mode"
            (if display then
                "true"

             else
                "false"
            )
        ]
        []


inline : String -> Html msg
inline =
    tex False
