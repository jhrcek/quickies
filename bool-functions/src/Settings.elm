module Settings exposing
    ( FormulaStyle(..)
    , Settings
    , default
    , joinConjuncts
    , overbar
    , viewNegation
    , viewTerm
    )

import Html exposing (Html)
import Html.Attributes as HA


type FormulaStyle
    = Symbols -- ¬x, x∧y
    | Compact -- x̄, xy


type alias Settings =
    { formula : FormulaStyle
    }


default : Settings
default =
    { formula = Compact
    }


{-| A string drawn with a CSS overline. Unlike a combining macron, the bar is
positioned by the line box, so it stays at a constant height regardless of the
glyph beneath it.
-}
overbar : String -> Html msg
overbar s =
    Html.span [ HA.style "text-decoration" "overline" ] [ Html.text s ]


{-| Render the negation of a (single-character) variable name according to the
chosen style: `¬x` or an overlined `x`.
-}
viewNegation : Settings -> String -> Html msg
viewNegation settings name =
    case settings.formula of
        Symbols ->
            Html.text ("¬" ++ name)

        Compact ->
            overbar name


{-| Combine already-rendered conjuncts using the chosen style: `x∧y` or `xy`.
-}
joinConjuncts : Settings -> List (Html msg) -> List (Html msg)
joinConjuncts settings parts =
    case settings.formula of
        Symbols ->
            List.intersperse (Html.text "∧") parts

        Compact ->
            parts


{-| Re-render a static label that may contain negation/conjunction notation
(`¬` and/or `∧`) so it respects the current settings. Labels without either
symbol render verbatim, as do labels whose `∧`-split yields any empty operand
(e.g. the standalone `∧` function name).
-}
viewTerm : Settings -> String -> Html msg
viewTerm settings s =
    if String.contains "∧" s || String.contains "¬" s then
        let
            operands =
                String.split "∧" s
                    |> List.map String.trim
        in
        if List.all (not << String.isEmpty) operands then
            operands
                |> List.map (viewOperand settings)
                |> joinConjuncts settings
                |> Html.span []

        else
            Html.text s

    else
        Html.text s


{-| Render a single operand, applying the negation style when it starts with a
leading `¬`.
-}
viewOperand : Settings -> String -> Html msg
viewOperand settings operand =
    case String.uncons operand of
        Just ( '¬', rest ) ->
            viewNegation settings rest

        _ ->
            Html.text operand
