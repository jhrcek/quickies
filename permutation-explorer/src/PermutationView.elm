module PermutationView exposing
    ( GraphMode(..)
    , cycleTypeToString
    , viewCard
    , viewCharacteristics
    , viewGraph
    , viewPermutation
    )

{-| Shared view helpers for displaying permutations.
-}

import GraphViz as GV
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Permutation


{-| Graph visualization mode.
-}
type GraphMode
    = CycleGraphMode
    | BipartiteGraphMode


{-| Wrap content in a standard card/panel container.
-}
viewCard : List (Html msg) -> Html msg
viewCard content =
    Html.div
        [ style "width" "350px"
        , style "border" "1px solid #ddd"
        , style "border-radius" "8px"
        , style "padding" "16px"
        , style "background" "#fff"
        ]
        content


{-| Format a cycle type (partition) as a string like "[3,2,1]".
-}
cycleTypeToString : List Int -> String
cycleTypeToString parts =
    "[" ++ String.join "," (List.map String.fromInt parts) ++ "]"


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
                |> cycleTypeToString

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
        , characteristic "Rank:" (String.fromInt (Permutation.toRank perm))
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
viewGraph : { mode : GraphMode, onToggle : msg } -> Permutation.Permutation -> Html msg
viewGraph config perm =
    let
        ( graph, engine ) =
            case config.mode of
                CycleGraphMode ->
                    ( Permutation.toCycleGraph perm, GV.Circo )

                BipartiteGraphMode ->
                    ( Permutation.toBipartiteGraph perm, GV.Dot )

        toggleLabel =
            case config.mode of
                CycleGraphMode ->
                    "Bipartite"

                BipartiteGraphMode ->
                    "Cycle"
    in
    Html.div
        [ style "background" "#f5f5f5"
        , style "padding" "12px"
        , style "border-radius" "8px"
        , style "text-align" "center"
        , style "position" "relative"
        ]
        [ Html.button
            [ onClick config.onToggle
            , style "position" "absolute"
            , style "top" "8px"
            , style "right" "8px"
            , style "padding" "4px 8px"
            , style "border" "1px solid #ccc"
            , style "border-radius" "4px"
            , style "background" "#fff"
            , style "cursor" "pointer"
            , style "font-size" "12px"
            ]
            [ Html.text toggleLabel ]
        , GV.graphviz engine graph
        ]


{-| Display a complete permutation card with label, characteristics, and graph.
-}
viewPermutation :
    { label : String
    , graphMode : GraphMode
    , onToggleGraph : msg
    }
    -> Permutation.Permutation
    -> Html msg
viewPermutation config perm =
    viewCard
        [ Html.h2 [ style "margin-top" "0" ] [ Html.text config.label ]
        , viewCharacteristics perm
        , viewGraph
            { mode = config.graphMode
            , onToggle = config.onToggleGraph
            }
            perm
        ]
