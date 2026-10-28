module BoolFun.View exposing
    ( algebraicNormalFormTex
    , boolCell
    , boolCellWith
    , boolColor
    , functionLabelTex
    , highlightCellAttrs
    , implicantTex
    , monomialTex
    , showBool
    , truthTable
    , viewAlgebraicNormalForm
    , viewImplicant
    , viewVarName
    )

{-| Html rendering of Boolean functions: truth tables, cells, implicants and
algebraic normal forms.
-}

import Bitwise
import BoolFun exposing (BF, Implicant, Literal(..), Monomial, Restriction, arityOf, eval, implicantToFunction, implicantToLiterals, inputBits, varNames)
import Html exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as Events
import KaTeX
import Settings exposing (Settings)


{-| TeX for the generic, name-free label of a function of the given arity, e.g.
`f(a,b,c)`. Used as the truth-table output-column header for arities that have
no meaningful per-function names.
-}
functionLabelTex : Int -> String
functionLabelTex n =
    "f(" ++ String.join "," (varNames n) ++ ")"


{-| A typeset variable name, e.g. in a table header.
-}
viewVarName : String -> Html msg
viewVarName =
    KaTeX.inline


truthTable : (Int -> msg) -> Settings -> Maybe Restriction -> List Implicant -> BF -> Html msg
truthTable flipBitInFunctionIndex settings maybeRestriction implicants bf =
    let
        arity =
            arityOf bf

        rowCount =
            2 ^ arity

        doubleBorder =
            A.style "border-left" "3px double #333"

        -- A row belongs to the restriction f|xᵥ=value when variable v's input
        -- bit equals the fixed value.
        rowInRestriction i =
            case maybeRestriction of
                Just { varIndex, value } ->
                    List.head (List.drop (varIndex - 1) (inputBits arity i)) == Just value

                Nothing ->
                    False

        -- Variable columns are 0-based; column c shows variable (c + 1). We
        -- highlight the free variables and the result, but leave the restricted
        -- variable's own column untouched (its value is constant on those rows).
        highlightVarCell c i =
            case maybeRestriction of
                Just { varIndex } ->
                    rowInRestriction i && c + 1 /= varIndex

                Nothing ->
                    False

        highlightIf cond b =
            if cond then
                highlightCellAttrs b

            else
                []

        implicantFns =
            List.map implicantToFunction implicants

        firstImplicantBorder idx =
            if idx == 0 then
                [ doubleBorder ]

            else
                []

        implicantHeaderCells =
            List.indexedMap
                (\idx impl ->
                    Html.th
                        (firstImplicantBorder idx)
                        [ viewImplicant settings impl ]
                )
                implicants

        implicantBodyCells i =
            List.indexedMap
                (\idx implFn ->
                    boolCellWith
                        (firstImplicantBorder idx)
                        (eval implFn i)
                )
                implicantFns
    in
    Html.table [ A.class "truth-table" ]
        [ Html.thead []
            [ Html.tr []
                (List.map (\l -> Html.th [] [ viewVarName l ]) (varNames arity)
                    ++ implicantHeaderCells
                    ++ [ Html.th [ doubleBorder ]
                            [ case BoolFun.name bf of
                                Just name ->
                                    Settings.viewFunctionName settings name

                                Nothing ->
                                    KaTeX.inline (functionLabelTex arity)
                            ]
                       ]
                )
            ]
        , Html.tbody []
            (List.range 0 (rowCount - 1)
                |> List.map
                    (\i ->
                        Html.tr []
                            (List.indexedMap
                                (\c b -> boolCellWith (highlightIf (highlightVarCell c i) b) b)
                                (inputBits arity i)
                                ++ implicantBodyCells i
                                ++ [ boolCellWith
                                        -- TODO fix width/height to prevent jumping when changing to different functions
                                        ([ Events.onClick (flipBitInFunctionIndex i)
                                         , doubleBorder
                                         ]
                                            ++ highlightIf (rowInRestriction i) (eval bf i)
                                        )
                                        (eval bf i)
                                   ]
                            )
                    )
            )
        ]


boolCell : Bool -> Html a
boolCell b =
    boolCellWith [] b


boolCellWith : List (Attribute a) -> Bool -> Html a
boolCellWith attrs b =
    Html.td
        (A.style "background-color" (boolColor b) :: attrs)
        [ Html.text (showBool b) ]


boolColor : Bool -> String
boolColor b =
    if b then
        -- mint
        "#A8DF9E"

    else
        -- rose
        "#F2A69E"


{-| A more vivid version of `boolColor`, used to highlight the cells of a
restricted sub-function in the truth table.
-}
highlightColor : Bool -> String
highlightColor b =
    if b then
        -- vivid green
        "#49C754"

    else
        -- vivid coral
        "#FF5A4E"


{-| Attributes giving a cell the restriction-highlight look: a vivid background
plus an inset outline (an outline, not a thicker border, so it never changes the
cell's size and can't cause layout flicker). Shared by the truth table and the
restriction-summary table so both highlight identically.
-}
highlightCellAttrs : Bool -> List (Attribute a)
highlightCellAttrs b =
    [ A.style "background-color" (highlightColor b)
    , A.style "outline" "2px solid #000"
    , A.style "outline-offset" "-2px"
    ]


showBool : Bool -> String
showBool b =
    if b then
        "True"

    else
        "False"


{-| Typeset form of an implicant: a conjunction of literals using the variable
names from `varNames`, or `⊤` for the empty implicant.
-}
viewImplicant : Settings -> Implicant -> Html msg
viewImplicant settings implicant =
    KaTeX.inline (implicantTex settings implicant)


implicantTex : Settings -> Implicant -> String
implicantTex settings implicant =
    let
        literals =
            implicantToLiterals implicant

        parts =
            List.map2
                (\name lit ->
                    case lit of
                        Positive ->
                            Just name

                        Negative ->
                            Just (Settings.negationTex settings name)

                        DontCare ->
                            Nothing
                )
                (varNames (List.length literals))
                literals
                |> List.filterMap identity
    in
    if List.isEmpty parts then
        "\\top"

    else
        Settings.conjunctionTex settings parts


{-| Typeset algebraic normal form, see `algebraicNormalFormTex`.
-}
viewAlgebraicNormalForm : Settings -> Int -> List Monomial -> Html msg
viewAlgebraicNormalForm settings arity monomials =
    KaTeX.inline (algebraicNormalFormTex settings arity monomials)


{-| An algebraic normal form as an XOR of monomials, e.g. `1 ⊕ a ⊕ b∧c`
(or `1 ⊕ a ⊕ bc` in the compact style). The empty polynomial is `0`.
-}
algebraicNormalFormTex : Settings -> Int -> List Monomial -> String
algebraicNormalFormTex settings arity monomials =
    if List.isEmpty monomials then
        "0"

    else
        monomials
            |> List.map (monomialTex settings arity)
            |> String.join " \\oplus "


{-| A single ANF monomial: the conjunction of its variables, or `1` for the
empty monomial.
-}
monomialTex : Settings -> Int -> Monomial -> String
monomialTex settings arity monomial =
    let
        names =
            varNames arity
                |> List.indexedMap Tuple.pair
                |> List.filter
                    (\( i, _ ) -> Bitwise.and monomial (Bitwise.shiftLeftBy (arity - 1 - i) 1) /= 0)
                |> List.map Tuple.second
    in
    if List.isEmpty names then
        "1"

    else
        Settings.conjunctionTex settings names
