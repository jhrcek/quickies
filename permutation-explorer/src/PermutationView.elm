module PermutationView exposing
    ( viewCard
    , viewCharacteristics
    , viewCycleNotation
    , viewGraph
    )

{-| Shared view helpers for displaying permutations.
-}

import GraphViz as GV
import Html exposing (Html)
import Html.Attributes exposing (style)
import Permutation


{-| Wrap content in a standard card/panel container.
-}
viewCard : List (Html msg) -> Html msg
viewCard content =
    Html.div
        [ style "flex" "1"
        , style "min-width" "250px"
        , style "border" "1px solid #ddd"
        , style "border-radius" "8px"
        , style "padding" "16px"
        , style "background" "#fff"
        ]
        content


{-| Display the cycle notation of a permutation (read-only).
-}
viewCycleNotation : Permutation.Permutation -> Html msg
viewCycleNotation perm =
    Html.div
        [ style "margin-bottom" "12px"
        , style "display" "flex"
        , style "align-items" "center"
        , style "gap" "8px"
        , style "padding" "8px 12px"
        , style "background" "#e8e8e8"
        , style "border-radius" "4px"
        , style "font-family" "monospace"
        , style "font-size" "16px"
        , style "min-height" "40px"
        ]
        [ Html.span [] [ Html.text (Permutation.toCyclesString perm) ] ]


{-| Display computed characteristics of a permutation.
-}
viewCharacteristics : Permutation.Permutation -> Html msg
viewCharacteristics perm =
    let
        signStr =
            case Permutation.sign perm of
                Permutation.Even ->
                    "+1 (even)"

                Permutation.Odd ->
                    "-1 (odd)"

        cycleTypeStr =
            Permutation.cycleType perm
                |> List.map String.fromInt
                |> String.join ", "
                |> (\s -> "[" ++ s ++ "]")

        characteristic : String -> String -> Html msg
        characteristic label val =
            Html.div
                [ style "display" "flex"
                , style "justify-content" "space-between"
                , style "gap" "8px"
                ]
                [ Html.span [ style "color" "#666" ] [ Html.text label ]
                , Html.span [ style "font-family" "monospace" ] [ Html.text val ]
                ]
    in
    Html.div
        [ style "font-size" "12px"
        , style "margin-bottom" "12px"
        , style "padding" "8px 12px"
        , style "background" "#f9f9f9"
        , style "border-radius" "4px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "4px"
        ]
        [ characteristic "Sign:" signStr
        , characteristic "Cycle type:" cycleTypeStr
        ]


{-| Display the graph visualization of a permutation.
-}
viewGraph : Maybe String -> Permutation.Permutation -> Html msg
viewGraph edgeColor perm =
    Html.div
        [ style "background" "#f5f5f5"
        , style "padding" "12px"
        , style "border-radius" "8px"
        , style "text-align" "center"
        ]
        [ GV.graphviz GV.Circo (Permutation.toCycleGraph edgeColor perm) ]
