module KaTeX exposing (inline)

{-| Wrapper for the `<math-tex>` custom element (defined in `index.html`), which
renders a TeX formula with KaTeX.
-}

import Html exposing (Html, node)
import Html.Attributes exposing (attribute)


inline : String -> Html msg
inline formula =
    node "math-tex"
        [ attribute "formula" formula
        , attribute "display-mode" "false"
        ]
        []
