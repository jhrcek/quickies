module ImplicationGraph exposing (Config, view)

{-| The implication graph of a CDCL trail, rendered with Graphviz.

One node per assigned literal, boxed by decision level. Each forced literal
gets an arrow from every literal that falsified the rest of its reason
clause. A conflict adds the node κ with arrows from the assignments that
falsified the conflict clause.

Styling is done in index.html via the `class` attributes set here.

-}

import GraphViz as G exposing (str)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Sat.Formula exposing (Clause, Literal)
import Sat.Trail as T exposing (Entry)
import Ui


{-| `frontier` is the clause currently being analysed (its literals are all
false), `resolved` the variables already resolved away, `uip` the 1UIP once
the analysis has finished.
-}
type alias Config =
    { trail : List Entry
    , level : Int
    , conflict : Maybe Clause
    , frontier : Clause
    , resolved : List Int
    , uip : Maybe Literal
    }


nodeId : Int -> String
nodeId v =
    "v" ++ String.fromInt v


conflictId : String
conflictId =
    "kappa"


view : Config -> Html msg
view cfg =
    let
        frontierVars =
            List.map abs cfg.frontier

        isFrontier v =
            List.member v frontierVars

        isResolved v =
            List.member v cfg.resolved

        classes e =
            String.join " "
                (List.filterMap identity
                    [ Just
                        (case e.kind of
                            T.Implied _ ->
                                "n-implied"

                            _ ->
                                "n-decision"
                        )
                    , when (isFrontier (abs e.lit)) "n-frontier"
                    , when (isResolved (abs e.lit)) "n-resolved"
                    , when (Maybe.map abs cfg.uip == Just (abs e.lit)) "n-uip"
                    ]
                )

        entryNode e =
            { name = nodeId (abs e.lit)
            , attributes =
                [ ( "label", str (Ui.litName e.lit) )
                , ( "class", str (classes e) )
                , ( "shape"
                  , str
                        (case e.kind of
                            T.Implied _ ->
                                "ellipse"

                            _ ->
                                "box"
                        )
                  )
                ]
                    ++ (if Maybe.map abs cfg.uip == Just (abs e.lit) then
                            [ ( "peripheries", str "2" ) ]

                        else
                            []
                       )
            }

        -- An edge crosses the cut when it leaves the frontier towards the
        -- conflict side (κ or a node already resolved away).
        edge tailVar headId headIsConflictSide =
            { tail = nodeId tailVar
            , head = headId
            , attributes =
                [ ( "class"
                  , str
                        (if isFrontier tailVar && headIsConflictSide then
                            "e-cut"

                         else if isResolved tailVar then
                            "e-resolved"

                         else
                            "e-plain"
                        )
                  )
                ]
            }

        reasonEdges e =
            case e.kind of
                T.Implied reason ->
                    reason
                        |> List.filter (\l -> abs l /= abs e.lit)
                        |> List.map (\l -> edge (abs l) (nodeId (abs e.lit)) (isResolved (abs e.lit)))

                _ ->
                    []

        conflictParts =
            case cfg.conflict of
                Just c ->
                    ( [ { name = conflictId
                        , attributes = [ ( "label", str "κ" ), ( "class", str "n-conflict" ), ( "shape", str "octagon" ) ]
                        }
                      ]
                    , List.map (\l -> edge (abs l) conflictId True) c
                    )

                Nothing ->
                    ( [], [] )

        cluster ( lvl, entries ) =
            { name = "cluster_L" ++ String.fromInt lvl
            , graphAttributes =
                [ ( "label", str ("level " ++ String.fromInt lvl) )
                , ( "class"
                  , str
                        (if lvl == cfg.level then
                            "c-current"

                         else
                            "c-level"
                        )
                  )
                , ( "labeljust", str "l" )
                ]
            , nodes = List.map entryNode entries
            }

        graph =
            { graphAttributes =
                [ ( "rankdir", str "LR" )
                , ( "bgcolor", str "transparent" )
                , ( "fontname", str "Inter" )
                , ( "fontsize", str "11" )
                , ( "nodesep", str "0.25" )
                , ( "ranksep", str "0.35" )
                ]
            , nodeAttributes = [ ( "fontname", str "JetBrains Mono" ), ( "fontsize", str "13" ), ( "height", str "0.35" ), ( "margin", str "0.06,0.03" ) ]
            , edgeAttributes = [ ( "arrowsize", str "0.6" ) ]
            , nodes = Tuple.first conflictParts
            , edges = List.concatMap reasonEdges (List.reverse cfg.trail) ++ Tuple.second conflictParts
            , subgraphs = List.map cluster (T.byLevel cfg.trail)
            }
    in
    div [ class "fig" ]
        [ G.graphviz "ig" graph
        , div [ class "legend" ]
            ([ key "k-decision" "decision"
             , key "k-implied" "forced"
             ]
                ++ (if cfg.conflict == Nothing then
                        []

                    else
                        [ key "k-frontier" "in the clause being analysed"
                        , key "k-resolved" "resolved away"
                        ]
                   )
                ++ (if cfg.uip == Nothing then
                        []

                    else
                        [ key "k-uip" "1UIP" ]
                   )
            )
        ]


key : String -> String -> Html msg
key cls label =
    span [ class "ig-key" ] [ span [ class ("ig-swatch " ++ cls) ] [], text label ]


when : Bool -> a -> Maybe a
when cond x =
    if cond then
        Just x

    else
        Nothing
