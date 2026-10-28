module BoolFun.View exposing
    ( boolCell
    , boolCellWith
    , boolColor
    , highlightCellAttrs
    , showBool
    , truthTable
    , viewImplicant
    )

{-| Html rendering of Boolean functions: truth tables, cells and implicants.
-}

import BoolFun exposing (BF, Implicant, Literal(..), Restriction, arityOf, eval, implicantToFunction, implicantToLiterals, inputBits, varNames)
import Html exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as Events
import Settings exposing (Settings)


{-| Generic, name-free label for a function of the given arity, e.g. `f(a,b,c)`.
Used as the truth-table output-column header for arities that have no meaningful
per-function names.
-}
genericName : Int -> String
genericName n =
    "f(" ++ String.join "," (varNames n) ++ ")"


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
                (List.map (\l -> Html.th [] [ Html.text l ]) (varNames arity)
                    ++ implicantHeaderCells
                    ++ [ Html.th [ doubleBorder ]
                            [ Settings.viewTerm settings
                                (BoolFun.name bf |> Maybe.withDefault (genericName arity))
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


{-| Human-readable form of an implicant: a conjunction of literals using the
variable names from `varNames`, or `⊤` for the empty implicant.
-}
viewImplicant : Settings -> Implicant -> Html msg
viewImplicant settings implicant =
    let
        literals =
            implicantToLiterals implicant

        parts =
            List.map2
                (\name lit ->
                    case lit of
                        Positive ->
                            Just (Html.text name)

                        Negative ->
                            Just (Settings.viewNegation settings name)

                        DontCare ->
                            Nothing
                )
                (varNames (List.length literals))
                literals
                |> List.filterMap identity
    in
    if List.isEmpty parts then
        Html.text "⊤"

    else
        Html.span [] (Settings.joinConjuncts settings parts)
