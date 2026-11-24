module Settings exposing
    ( BoolStyle(..)
    , FormulaStyle(..)
    , Settings
    , conjunctionTex
    , default
    , disjunctionTex
    , negationTex
    , showBool
    , showBoolInFormula
    , viewFunctionName
    )

import Html exposing (Html)
import KaTeX


type FormulaStyle
    = Symbols -- ¬x, x∧y
    | Compact -- x̄, xy


type BoolStyle
    = Words -- False / True
    | Digits -- 0 / 1
    | Marks -- ✗ / ✓
    | ColorOnly -- no text, just the cell's background color


type alias Settings =
    { formula : FormulaStyle
    , bool : BoolStyle
    }


default : Settings
default =
    { formula = Symbols
    , bool = Words
    }


{-| Display a boolean value according to the chosen style.
-}
showBool : Settings -> Bool -> String
showBool settings b =
    case ( settings.bool, b ) of
        ( Words, True ) ->
            "True"

        ( Words, False ) ->
            "False"

        ( Digits, True ) ->
            "1"

        ( Digits, False ) ->
            "0"

        ( Marks, True ) ->
            "✓"

        ( Marks, False ) ->
            "✗"

        ( ColorOnly, _ ) ->
            ""


{-| Like `showBool`, but for places with no background color to carry the value
(e.g. inside a formula), where `ColorOnly` falls back to `0 / 1`.
-}
showBoolInFormula : Settings -> Bool -> String
showBoolInFormula settings =
    case settings.bool of
        ColorOnly ->
            showBool { settings | bool = Digits }

        _ ->
            showBool settings


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
