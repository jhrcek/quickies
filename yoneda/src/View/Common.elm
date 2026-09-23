module View.Common exposing (countBadge, cycleNotation, elementPicker, lawBadge)

{-| Small view pieces shared by several chapters.
-}

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KaTeX
import Math.FinFunction as FinFunction exposing (FinFunction)
import Math.Group as Group exposing (Group)


{-| "holds" / "FAILS" badge for a law check.
-}
lawBadge : Bool -> Html msg
lawBadge ok =
    if ok then
        span [ class "badge ok" ] [ text "holds" ]

    else
        span [ class "badge bad" ] [ text "FAILS" ]


{-| Badge comparing the sizes of the two sides of a bijection.
-}
countBadge : Int -> Int -> Html msg
countBadge left right =
    if left == right then
        span [ class "badge ok" ] [ text (String.fromInt left ++ " = " ++ String.fromInt right) ]

    else
        span [ class "badge bad" ] [ text (String.fromInt left ++ " ≠ " ++ String.fromInt right) ]


{-| One button per group element, the current one active.
-}
elementPicker : (Int -> msg) -> Group -> Int -> Html msg
elementPicker toMsg grp current =
    div [ class "controls" ]
        (List.range 0 (Group.order grp - 1)
            |> List.map
                (\i -> button [ classList [ ( "active", i == current ) ], onClick (toMsg i) ] [ KaTeX.inline (Group.label grp i) ])
        )


{-| TeX cycle notation of a permutation of the group's elements, labelled by the elements.
-}
cycleNotation : Group -> FinFunction -> String
cycleNotation grp f =
    let
        nontrivial =
            FinFunction.cycles f |> List.filter (\c -> List.length c > 1)
    in
    if List.isEmpty nontrivial then
        "\\mathrm{id}"

    else
        nontrivial
            |> List.map (\c -> "(" ++ String.join "\\;" (List.map (Group.label grp) c) ++ ")")
            |> String.concat
