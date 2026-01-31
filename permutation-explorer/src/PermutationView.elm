module PermutationView exposing
    ( GraphMode(..)
    , cycleTypeToString
    , viewCard
    , viewCharacteristics
    , viewDerivationGraph
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
import Route
import ViewHelpers


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


{-| Format a cycle type as a string like "[3,2,1]".
-}
cycleTypeToString : List Int -> String
cycleTypeToString parts =
    "[" ++ String.join "," (List.map String.fromInt parts) ++ "]"


{-| Format Lehmer digits as a string like "[2,1,0]".
-}
lehmerDigitsToString : List Int -> String
lehmerDigitsToString digits =
    "[" ++ String.join "," (List.map String.fromInt digits) ++ "]"


{-| Format one-line notation as a string like "0 3 2 1 4".
-}
oneLineToString : List Int -> String
oneLineToString elements =
    String.join " " (List.map String.fromInt elements)


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

        cycleType =
            Permutation.cycleType perm

        cycleTypeStr =
            cycleTypeToString cycleType

        cycleTypeRoute =
            Route.Group (Permutation.getSize perm)
                (Route.ConjugacyClasses (Route.ConjugacyClass cycleType))

        orderStr =
            String.fromInt (Permutation.order perm)

        characteristic : String -> Html msg -> Html msg
        characteristic label valElem =
            Html.div
                [ style "display" "flex"
                , style "justify-content" "space-between"
                , style "gap" "8px"
                ]
                [ Html.span [ style "color" "#666" ] [ Html.text label ]
                , valElem
                ]

        textCharacteristic : String -> String -> Html msg
        textCharacteristic label value =
            characteristic label
                (Html.span [ style "font-family" "monospace" ]
                    [ Html.text value ]
                )

        linkCharacteristic : String -> Route.Route -> String -> Html msg
        linkCharacteristic label route value =
            characteristic label
                (ViewHelpers.routeLink [ style "font-family" "monospace" ] route value)

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
        [ textCharacteristic "Rank:" (String.fromInt (Permutation.toRank perm))
        , textCharacteristic "Lehmer digits:" (lehmerDigitsToString (Permutation.toLehmerDigits perm))
        , textCharacteristic "Inversion count:" (String.fromInt (Permutation.inversionCount perm))
        , textCharacteristic "One-line notation:" (oneLineToString (Permutation.toOneLineNotation perm))
        , textCharacteristic "Cycle notation:" (Permutation.toCyclesString perm)
        , linkCharacteristic "Cycle type:" cycleTypeRoute cycleTypeStr
        , textCharacteristic "# of cycles:" (String.fromInt (Permutation.numCycles perm))
        , textCharacteristic "# of fixed points:" (String.fromInt (Permutation.numFixedPoints perm))
        , textCharacteristic "Sign:" signStr
        , textCharacteristic "Order:" orderStr
        , textCharacteristic "Centralizer size:" (String.fromInt (Permutation.centralizerSize perm))
        , textCharacteristic "Conjugacy class size:" (String.fromInt (Permutation.conjugacyClassSize perm))
        , textCharacteristic "Is identity:" (boolStr (Permutation.isIdentity perm))
        , textCharacteristic "Is involution:" (boolStr (Permutation.isInvolution perm))
        , textCharacteristic "Is derangement:" (boolStr (Permutation.isDerangement perm))
        , textCharacteristic "Is transposition:" (boolStr (Permutation.isTransposition perm))
        , textCharacteristic "Is cyclic:" (boolStr (Permutation.isCyclic perm))
        ]


{-| Build a GraphViz graph showing how permutation properties derive from each other.
-}
toDerivationGraph : Permutation.Permutation -> GV.Graph
toDerivationGraph perm =
    let
        cycleTypeStr =
            cycleTypeToString (Permutation.cycleType perm)

        boolStr b =
            if b then
                "Yes"

            else
                "No"

        signStr =
            case Permutation.sign perm of
                Permutation.Even ->
                    "+1 (even)"

                Permutation.Odd ->
                    "-1 (odd)"

        lehmerDigitsStr =
            lehmerDigitsToString (Permutation.toLehmerDigits perm)

        nodes =
            [ { name = "cycles", attributes = [ ( "label", GV.str ("Cycles\\n" ++ Permutation.toCyclesString perm) ) ] }
            , { name = "cycleType", attributes = [ ( "label", GV.str ("Cycle Type\\n" ++ cycleTypeStr) ) ] }
            , { name = "numCycles", attributes = [ ( "label", GV.str ("# Cycles\\n" ++ String.fromInt (Permutation.numCycles perm)) ) ] }
            , { name = "numFixed", attributes = [ ( "label", GV.str ("# Fixed Pts\\n" ++ String.fromInt (Permutation.numFixedPoints perm)) ) ] }
            , { name = "order", attributes = [ ( "label", GV.str ("Order\\n" ++ String.fromInt (Permutation.order perm)) ) ] }
            , { name = "sign", attributes = [ ( "label", GV.str ("Sign\\n" ++ signStr) ) ] }
            , { name = "isIdentity", attributes = [ ( "label", GV.str ("Identity\\n" ++ boolStr (Permutation.isIdentity perm)) ) ] }
            , { name = "isDerangement", attributes = [ ( "label", GV.str ("Derangement\\n" ++ boolStr (Permutation.isDerangement perm)) ) ] }
            , { name = "isInvolution", attributes = [ ( "label", GV.str ("Involution\\n" ++ boolStr (Permutation.isInvolution perm)) ) ] }
            , { name = "isCyclic", attributes = [ ( "label", GV.str ("Cyclic\\n" ++ boolStr (Permutation.isCyclic perm)) ) ] }
            , { name = "isTransposition", attributes = [ ( "label", GV.str ("Transposition\\n" ++ boolStr (Permutation.isTransposition perm)) ) ] }
            , { name = "lehmerDigits", attributes = [ ( "label", GV.str ("Lehmer Digits\\n" ++ lehmerDigitsStr) ) ] }
            , { name = "rank", attributes = [ ( "label", GV.str ("Rank\\n" ++ String.fromInt (Permutation.toRank perm)) ) ] }
            , { name = "inversionCount", attributes = [ ( "label", GV.str ("Inversion Count\\n" ++ String.fromInt (Permutation.inversionCount perm)) ) ] }
            ]

        edges =
            [ { tail = "cycles", head = "cycleType", attributes = [ ( "label", GV.str "lengths" ) ] }
            , { tail = "cycleType", head = "numCycles", attributes = [ ( "label", GV.str "count parts" ) ] }
            , { tail = "cycleType", head = "numFixed", attributes = [ ( "label", GV.str "count 1s" ) ] }
            , { tail = "cycleType", head = "order", attributes = [ ( "label", GV.str "LCM" ) ] }
            , { tail = "cycleType", head = "isInvolution", attributes = [ ( "label", GV.str "all ≤ 2" ) ] }
            , { tail = "cycleType", head = "isCyclic", attributes = [ ( "label", GV.str "single n-cycle" ) ] }
            , { tail = "cycleType", head = "isTransposition", attributes = [ ( "label", GV.str "one 2, rest 1s" ) ] }
            , { tail = "numCycles", head = "sign", attributes = [ ( "label", GV.str "(-1)^(n-k)" ) ] }
            , { tail = "numFixed", head = "isIdentity", attributes = [ ( "label", GV.str "= n" ) ] }
            , { tail = "numFixed", head = "isDerangement", attributes = [ ( "label", GV.str "= 0" ) ] }
            , { tail = "lehmerDigits", head = "inversionCount", attributes = [ ( "label", GV.str "sum" ) ] }
            , { tail = "lehmerDigits", head = "rank", attributes = [ ( "label", GV.str "factorial base" ) ] }
            ]
    in
    { name = Just "Derivations"
    , directed = True
    , graphAttributes =
        [ ( "rankdir", GV.str "TB" )
        , ( "nodesep", GV.num 0.3 )
        , ( "ranksep", GV.num 0.4 )
        ]
    , nodeAttributes =
        [ ( "shape", GV.str "box" )
        , ( "style", GV.str "filled" )
        , ( "fillcolor", GV.str "#f0f0f0" )
        , ( "fontsize", GV.num 10 )
        ]
    , edgeAttributes =
        [ ( "fontsize", GV.num 8 )
        , ( "arrowsize", GV.str "0.5" )
        , ( "fontcolor", GV.str "#666666" )
        ]
    , nodes = nodes
    , edges = edges
    , subgraphs = []
    }


{-| Display the derivation graph showing relationships between permutation properties.
-}
viewDerivationGraph : Permutation.Permutation -> Html msg
viewDerivationGraph perm =
    Html.div
        [ style "background" "#f5f5f5"
        , style "padding" "12px"
        , style "border-radius" "8px"
        , style "text-align" "center"
        ]
        [ GV.graphviz GV.Dot (toDerivationGraph perm) ]


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
