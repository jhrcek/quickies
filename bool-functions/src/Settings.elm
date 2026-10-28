module Settings exposing
    ( FormulaStyle(..)
    , Settings
    , conjunctionTex
    , default
    , disjunctionTex
    , negationTex
    , viewFunctionName
    )

import Html exposing (Html)
import KaTeX


type FormulaStyle
    = Symbols -- ¬x, x∧y
    | Compact -- x̄, xy



-- TODO make it configurable how to display each value (TRUE vs T vs unicode checkmark?)


type alias Settings =
    { formula : FormulaStyle
    }


default : Settings
default =
    { formula = Symbols }


{-| TeX for the negation of a (single-character) variable name according to the
chosen style: `¬x` or an overlined `x`.
-}
negationTex : Settings -> String -> String
negationTex settings name =
    case settings.formula of
        Symbols ->
            "\\neg " ++ name

        Compact ->
            "\\overline{" ++ name ++ "}"


{-| Combine TeX conjuncts using the chosen style: `x∧y` or `xy`.
-}
conjunctionTex : Settings -> List String -> String
conjunctionTex settings parts =
    case settings.formula of
        Symbols ->
            String.join " \\land " parts

        Compact ->
            String.concat parts


{-| Combine TeX disjuncts using the chosen style: `x∨y` or `x+y`.
-}
disjunctionTex : Settings -> List String -> String
disjunctionTex settings parts =
    case settings.formula of
        Symbols ->
            String.join " \\lor " parts

        Compact ->
            String.join " + " parts


{-| Render a conventional function name (see `BoolFun.name`). Word names like
`XOR` stay plain text; symbolic names like `¬a ∧ b` or `a → b` are typeset,
respecting the current settings for negation and conjunction.
-}
viewFunctionName : Settings -> String -> Html msg
viewFunctionName settings s =
    if String.all Char.isUpper s then
        Html.text s

    else
        KaTeX.inline (functionNameTex settings s)


{-| Names whose `∧`-split yields any empty operand (e.g. the standalone `∧`
function name) are translated symbol by symbol instead.
-}
functionNameTex : Settings -> String -> String
functionNameTex settings s =
    let
        operands =
            String.split "∧" s
                |> List.map String.trim
    in
    if List.all (not << String.isEmpty) operands then
        operands
            |> List.map (operandTex settings)
            |> conjunctionTex settings

    else
        symbolsTex s


{-| A single operand, applying the negation style when it starts with a leading `¬`.
-}
operandTex : Settings -> String -> String
operandTex settings operand =
    case String.uncons operand of
        Just ( '¬', rest ) ->
            negationTex settings rest

        _ ->
            symbolsTex operand


symbolsTex : String -> String
symbolsTex =
    String.replace "→" "\\to"
        >> String.replace "∨" "\\lor"
        >> String.replace "∧" "\\land"
        >> String.replace "¬" "\\neg "
