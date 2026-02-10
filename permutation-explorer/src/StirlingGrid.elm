module StirlingGrid exposing (view)

import Array exposing (Array)
import GraphViz exposing (Engine(..), Graph, emptyGraph, graphviz, num, simpleNode, str)
import Html exposing (Html)
import Html.Attributes



-- CONFIG


maxN : Int
maxN =
    9


mulColor : String
mulColor =
    "#c05020"


newColor : String
newColor =
    "#2060b0"



-- DATA


type alias Table =
    Array (Array Int)


buildTable : Int -> Table
buildTable n =
    let
        nextRow : Int -> Array Int -> Array Int
        nextRow row prev =
            Array.initialize (row + 2)
                (\k ->
                    row
                        * (Array.get k prev |> Maybe.withDefault 0)
                        + (Array.get (k - 1) prev |> Maybe.withDefault 0)
                )
    in
    List.range 0 (n - 1)
        |> List.foldl
            (\row acc ->
                Array.push
                    (nextRow row (Array.get row acc |> Maybe.withDefault Array.empty))
                    acc
            )
            (Array.fromList [ Array.fromList [ 1 ] ])


getVal : Table -> Int -> Int -> Int
getVal table row col =
    Array.get row table
        |> Maybe.andThen (Array.get col)
        |> Maybe.withDefault 0


formatInt : Int -> String
formatInt n =
    if n < 1000 then
        String.fromInt n

    else
        let
            s =
                String.fromInt n

            len =
                String.length s

            go i acc =
                if i >= len then
                    acc

                else
                    let
                        remaining =
                            len - i

                        sep =
                            if i > 0 && modBy 3 remaining == 0 then
                                ","

                            else
                                ""
                    in
                    go (i + 1) (acc ++ sep ++ String.slice i (i + 1) s)
        in
        go 0 ""



-- GRAPH BUILDING


nodeId : Int -> Int -> String
nodeId n k =
    "n" ++ String.fromInt n ++ "k" ++ String.fromInt k


rowHeaderId : Int -> String
rowHeaderId n =
    "rh" ++ String.fromInt n


buildGraph : Table -> Graph
buildGraph table =
    let
        cells =
            List.concatMap
                (\n -> List.map (\k -> ( n, k )) (List.range 0 n))
                (List.range 0 maxN)

        cellNodes =
            List.map
                (\( n, k ) ->
                    { name = nodeId n k
                    , attributes =
                        [ ( "label", str (formatInt (getVal table n k)) )
                        , ( "group", str ("col" ++ String.fromInt k) )
                        ]
                    }
                )
                cells

        rowHeaderNodes =
            List.map
                (\n ->
                    { name = rowHeaderId n
                    , attributes =
                        [ ( "label", str ("n=" ++ String.fromInt n) )
                        , ( "shape", str "plaintext" )
                        , ( "fontcolor", str "#999999" )
                        , ( "fontsize", str "11" )
                        , ( "width", str "0.35" )
                        ]
                    }
                )
                (List.range 0 maxN)

        recurrenceEdges =
            List.concatMap
                (\( n, k ) ->
                    if n >= maxN || getVal table n k == 0 then
                        []

                    else
                        let
                            mulEdge =
                                { tail = nodeId n k
                                , head = nodeId (n + 1) k
                                , attributes =
                                    [ ( "color", str mulColor )
                                    , ( "penwidth", num (max 0.8 (toFloat n * 0.5 + 0.5)) )
                                    ]
                                        ++ (if n > 0 then
                                                [ ( "label", str ("  ×" ++ String.fromInt n) )
                                                , ( "fontcolor", str mulColor )
                                                , ( "fontsize", str "9" )
                                                ]

                                            else
                                                []
                                           )
                                }

                            newEdge =
                                { tail = nodeId n k
                                , head = nodeId (n + 1) (k + 1)
                                , attributes =
                                    [ ( "color", str newColor )
                                    , ( "style", str "dashed" )
                                    , ( "penwidth", num 1.0 )
                                    ]
                                }
                        in
                        [ mulEdge, newEdge ]
                )
                cells

        orderingEdges =
            List.concatMap
                (\n ->
                    { tail = rowHeaderId n
                    , head = nodeId n 0
                    , attributes = [ ( "style", str "invis" ) ]
                    }
                        :: List.map
                            (\k ->
                                { tail = nodeId n k
                                , head = nodeId n (k + 1)
                                , attributes = [ ( "style", str "invis" ) ]
                                }
                            )
                            (List.range 0 (n - 1))
                )
                (List.range 0 maxN)

        rowSubgraphs =
            List.map
                (\n ->
                    { name = Nothing
                    , graphAttributes = [ ( "rank", str "same" ) ]
                    , nodes =
                        simpleNode (rowHeaderId n)
                            :: List.map (\k -> simpleNode (nodeId n k)) (List.range 0 n)
                    }
                )
                (List.range 0 maxN)
    in
    { emptyGraph
        | name = Just "stirling"
        , graphAttributes =
            [ ( "rankdir", str "TB" )
            , ( "nodesep", str "0.25" )
            , ( "ranksep", str "0.4" )
            ]
        , nodeAttributes =
            [ ( "shape", str "box" )
            , ( "style", str "rounded" )
            , ( "fontsize", str "11" )
            , ( "fontname", str "sans-serif" )
            , ( "color", str "#cccccc" )
            , ( "width", str "0.65" )
            , ( "height", str "0.35" )
            , ( "fixedsize", str "true" )
            ]
        , edgeAttributes =
            [ ( "fontname", str "sans-serif" ) ]
        , nodes = rowHeaderNodes ++ cellNodes
        , edges = recurrenceEdges ++ orderingEdges
        , subgraphs = rowSubgraphs
    }



-- VIEW


view : Html msg
view =
    let
        table =
            buildTable maxN

        graph =
            buildGraph table
    in
    Html.div []
        [ Html.p [ Html.Attributes.style "font-size" "0.85em", Html.Attributes.style "color" "#666" ]
            [ Html.text "c(n+1, k) = "
            , Html.span [ Html.Attributes.style "color" mulColor ] [ Html.text "n · c(n, k)" ]
            , Html.text " + "
            , Html.span [ Html.Attributes.style "color" newColor ] [ Html.text "c(n, k-1)" ]
            ]
        , graphviz Dot graph
        , Html.p [ Html.Attributes.style "font-size" "0.85em", Html.Attributes.style "color" "#666", Html.Attributes.style "margin-top" "8px" ]
            [ Html.span [ Html.Attributes.style "color" mulColor ] [ Html.text "——→" ]
            , Html.text " ×n — insert into existing cycle   "
            , Html.span [ Html.Attributes.style "color" newColor ] [ Html.text "- - →" ]
            , Html.text " ×1 — start new cycle"
            ]
        ]
