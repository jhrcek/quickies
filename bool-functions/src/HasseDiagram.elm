module HasseDiagram exposing
    ( maxRenderableArity
    , view
    )

import Bitwise
import BoolFun exposing (BF)
import GraphViz exposing (Engine(..))
import Html exposing (Html)
import Html.Attributes


maxRenderableArity : Int
maxRenderableArity =
    4


{-| Renders the Boolean cube {0,1}^n as a Hasse diagram with nodes
colored by f's output and (optionally) violating covering edges
highlighted.

A violation occurs on a covering pair (x, y) where y = x with one extra
1-bit and f(x)=1 but f(y)=0.

For arity > maxRenderableArity returns a textual fallback message.

-}
view : { highlightMonotonicityViolations : Bool } -> BF -> Html msg
view { highlightMonotonicityViolations } bf =
    let
        arity =
            BoolFun.arityOf bf
    in
    if arity > maxRenderableArity then
        Html.div
            [ Html.Attributes.style "color" "#666"
            , Html.Attributes.style "padding" "10px"
            ]
            [ Html.text
                ("Hasse diagram is not rendered for arity > "
                    ++ String.fromInt maxRenderableArity
                    ++ " — the cube has "
                    ++ String.fromInt (2 ^ arity)
                    ++ " nodes which doesn't lay out cleanly. Use the truth table above."
                )
            ]

    else
        GraphViz.graphviz Dot (buildGraph highlightMonotonicityViolations bf)


buildGraph : Bool -> BF -> GraphViz.Graph
buildGraph highlightViolations bf =
    let
        arity =
            BoolFun.arityOf bf

        allInputs =
            BoolFun.inputs arity

        nodeName i =
            "n" ++ String.fromInt i

        bitString i =
            BoolFun.inputBits arity i
                |> List.map
                    (\b ->
                        if b then
                            "1"

                        else
                            "0"
                    )
                |> String.concat

        node i =
            { name = nodeName i
            , attributes =
                [ ( "label", GraphViz.str (bitString i) )
                , ( "style", GraphViz.str "filled" )
                , ( "fillcolor"
                  , GraphViz.str (BoolFun.boolColor (BoolFun.eval bf i))
                  )
                , ( "shape", GraphViz.str "circle" )
                , ( "fontname", GraphViz.str "monospace" )
                ]
            }

        coveringEdges =
            allInputs
                |> List.concatMap
                    (\x ->
                        List.range 0 (arity - 1)
                            |> List.filterMap
                                (\k ->
                                    let
                                        mask =
                                            Bitwise.shiftLeftBy k 1
                                    in
                                    if Bitwise.and x mask == 0 then
                                        Just ( x, Bitwise.or x mask )

                                    else
                                        Nothing
                                )
                    )

        edge ( x, y ) =
            let
                isViolation =
                    highlightViolations
                        && BoolFun.eval bf x
                        && not (BoolFun.eval bf y)
            in
            { tail = nodeName x
            , head = nodeName y
            , attributes =
                if isViolation then
                    [ ( "color", GraphViz.str "red" )
                    , ( "penwidth", GraphViz.num 2.5 )
                    ]

                else
                    [ ( "color", GraphViz.str "#888" ) ]
            }

        -- group nodes by Hamming weight to get classic layered layout
        rankSubgraph weight =
            { name = Just ("rank_" ++ String.fromInt weight)
            , graphAttributes = [ ( "rank", GraphViz.str "same" ) ]
            , nodes =
                allInputs
                    |> List.filter (\i -> popCount i == weight)
                    |> List.map (\i -> { name = nodeName i, attributes = [] })
            }
    in
    { name = Just "hasse"
    , directed = True
    , graphAttributes =
        [ ( "rankdir", GraphViz.str "BT" )
        , ( "nodesep", GraphViz.num 0.4 )
        , ( "ranksep", GraphViz.num 0.5 )
        ]
    , nodeAttributes = []
    , edgeAttributes = [ ( "arrowhead", GraphViz.str "none" ) ]
    , nodes = List.map node allInputs
    , edges = List.map edge coveringEdges
    , subgraphs = List.map rankSubgraph (List.range 0 arity)
    }


popCount : Int -> Int
popCount n =
    if n <= 0 then
        0

    else
        Bitwise.and n 1 + popCount (Bitwise.shiftRightZfBy 1 n)
