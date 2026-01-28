module PermutationView exposing
    ( viewCard
    , viewCharacteristics
    , viewGraph
    , viewPermutation
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

        orderStr =
            String.fromInt (Permutation.order perm)

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

        boolStr b =
            if b then
                "Yes"

            else
                "No"
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
        [ characteristic "Cycles:" (Permutation.toCyclesString perm)
        , characteristic "Sign:" signStr
        , characteristic "Cycle type:" cycleTypeStr
        , characteristic "# of cycles:" (String.fromInt (Permutation.numCycles perm))
        , characteristic "# of fixed points:" (String.fromInt (Permutation.numFixedPoints perm))
        , characteristic "Order:" orderStr
        , characteristic "Lehmer code:" (String.fromInt (Permutation.toLehmerCode perm))
        , characteristic "Centralizer size:" (String.fromInt (Permutation.centralizerSize perm))
        , characteristic "Conjugacy class size:" (String.fromInt (Permutation.conjugacyClassSize perm))
        , characteristic "Is identity:" (boolStr (Permutation.isIdentity perm))
        , characteristic "Is involution:" (boolStr (Permutation.isInvolution perm))
        , characteristic "Is derangement:" (boolStr (Permutation.isDerangement perm))
        , characteristic "Is transposition:" (boolStr (Permutation.isTransposition perm))
        , characteristic "Is cyclic:" (boolStr (Permutation.isCyclic perm))
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


{-| Display a complete permutation card with label, characteristics, and graph.
-}
viewPermutation : String -> Maybe String -> Permutation.Permutation -> Html msg
viewPermutation label edgeColor perm =
    viewCard
        [ Html.h2 [ style "margin-top" "0" ] [ Html.text label ]
        , viewCharacteristics perm
        , viewGraph edgeColor perm
        ]
