module Lesson.Reductions exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import ListX
import Sat.Cdcl as C
import Sat.Encode as E exposing (Graph)
import Sat.Formula exposing (Clause)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Ui exposing (em, insight, p, strong, t, wiki)


type alias Model =
    { graph : Int
    , k : Int
    , result : C.State
    }


type Msg
    = PickGraph Int
    | PickK Int


init : Model
init =
    solve 1 3


solve : Int -> Int -> Model
solve gi k =
    { graph = gi, k = k, result = C.solve (E.coloringFormula (graphAt gi) k).formula }


graphAt : Int -> Graph
graphAt i =
    ListX.getAt i E.graphs |> Maybe.withDefault { name = "", labels = [], positions = [], edges = [] }


update : Msg -> Model -> Model
update msg m =
    case msg of
        PickGraph i ->
            solve i m.k

        PickK k ->
            solve m.graph k


colorNames : List String
colorNames =
    [ "red", "blue", "yellow", "green" ]


{-| Name the encoding's variables after what they mean: A·red, B·blue, ...
-}
meaning : Graph -> Int -> Int -> String
meaning g k lit =
    let
        v =
            abs lit - 1

        node =
            ListX.getAt (v // k) g.labels |> Maybe.withDefault "?"

        color =
            ListX.getAt (modBy k v) colorNames |> Maybe.withDefault "?"
    in
    (if lit < 0 then
        "¬"

     else
        ""
    )
        ++ node
        ++ "·"
        ++ color


clauseMeaning : Graph -> Int -> Clause -> String
clauseMeaning g k c =
    "(" ++ String.join " ∨ " (List.map (meaning g k) c) ++ ")"


view : Model -> Html Msg
view m =
    let
        g =
            graphAt m.graph

        enc =
            E.coloringFormula g m.k

        three =
            E.toThreeCnf enc.formula

        coloring =
            case m.result.status of
                C.Sat a ->
                    Just (E.decodeColoring g m.k a)

                _ ->
                    Nothing
    in
    div []
        [ p
            [ t "NP-completeness cuts both ways: SAT is hard, but it is also "
            , strong "universal"
            , t ". Any problem whose solutions are easy to check can be written as a formula. In practice that is how SAT solvers get used: you don't write a scheduler or a verifier, you write an "
            , em "encoding"
            , t " and let the solver do the search."
            ]
        , p [ t "Example: ", wiki "Graph coloring" "colour the nodes of a graph", t " with k colours so that neighbours always differ. One variable per (node, colour) pair, meaning “this node has this colour”, and three families of clauses:" ]
        , div [ class "chooser" ]
            (span [ class "chooser-l" ] [ text "graph" ]
                :: List.indexedMap (\i gr -> button [ class "chip", classList [ ( "on", i == m.graph ) ], onClick (PickGraph i) ] [ text gr.name ]) E.graphs
            )
        , div [ class "chooser" ]
            (span [ class "chooser-l" ] [ text "colours" ]
                :: List.map (\k -> button [ class "chip", classList [ ( "on", k == m.k ) ], onClick (PickK k) ] [ text (String.fromInt k) ]) [ 2, 3, 4 ]
            )
        , div [ class "fig graph" ] [ graphSvg g coloring ]
        , case coloring of
            Just _ ->
                Ui.banner "good" [ strong "SAT: ", text ("a proper " ++ String.fromInt m.k ++ "-colouring, decoded from the solver's model.") ]

            Nothing ->
                Ui.banner "bad" [ strong "UNSAT: ", text ("no " ++ String.fromInt m.k ++ "-colouring exists. CDCL proved it after " ++ String.fromInt m.result.stats.conflicts ++ " conflicts.") ]
        , div []
            (List.map
                (\grp ->
                    div [ class "panel enc" ]
                        [ div [ class "enc-h" ] [ strong grp.title, span [ class "enc-n" ] [ text (String.fromInt (List.length grp.clauses) ++ " clauses") ] ]
                        , div [ class "mono small" ]
                            (List.take 3 grp.clauses |> List.map (\c -> div [] [ text (clauseMeaning g m.k c) ]))
                        ]
                )
                enc.groups
            )
        , Ui.stats
            [ Ui.stat "variables" (String.fromInt enc.formula.numVars)
            , Ui.stat "clauses" (String.fromInt (List.length enc.formula.clauses))
            , Ui.stat "after splitting" (String.fromInt three.numVars ++ " v / " ++ String.fromInt (List.length three.clauses) ++ " c")
            ]
        , insight "From any SAT to 3-SAT"
            [ p [ t "The colouring clauses have 2 or k literals, not exactly 3. That's fine: any long clause (l₁ ∨ l₂ ∨ l₃ ∨ l₄) can be split with a fresh variable y into (l₁ ∨ l₂ ∨ y) ∧ (¬y ∨ l₃ ∨ l₄). Both have the same satisfiability: if some lᵢ is true, choose y to satisfy the other half. This is why “3” is the magic number: 3-SAT is as hard as SAT, while 2-SAT is easy. The last stat counts variables and clauses after splitting every clause longer than 3." ]
            ]
        , insight "Where encodings run the world"
            [ p [ t "Chip makers check hardware designs by encoding “is there an input that breaks this circuit within k clock cycles?” (", wiki "Model checking" "bounded model checking", t "). Linux package managers such as openSUSE's (via libsolv) resolve dependencies with a SAT solver. Sudoku is 729 variables. And in 2016 a SAT solver settled the ", wiki "Boolean Pythagorean triples problem" "Boolean Pythagorean triples problem", t ", producing a 200-terabyte proof." ]
            ]
        ]


palette : List String
palette =
    [ "#d9472b", "#2f6fd0", "#e6b422", "#2a8c5a" ]


graphSvg : Graph -> Maybe (List (Maybe Int)) -> Svg msg
graphSvg g coloring =
    let
        size =
            260

        pt ( x, y ) =
            ( 20 + x * (size - 40), 20 + y * (size - 40) )

        positions =
            List.map pt g.positions

        at i =
            ListX.getAt i positions |> Maybe.withDefault ( 0, 0 )

        colorOf i =
            coloring
                |> Maybe.andThen (ListX.getAt i)
                |> Maybe.andThen identity
                |> Maybe.andThen (\c -> ListX.getAt c palette)
                |> Maybe.withDefault "var(--card)"

        edges =
            List.map
                (\( a, b ) ->
                    let
                        ( x1, y1 ) =
                            at a

                        ( x2, y2 ) =
                            at b
                    in
                    Svg.line [ SA.x1 (String.fromFloat x1), SA.y1 (String.fromFloat y1), SA.x2 (String.fromFloat x2), SA.y2 (String.fromFloat y2), SA.class "g-edge" ] []
                )
                g.edges

        nodes =
            List.indexedMap
                (\i label ->
                    let
                        ( x, y ) =
                            at i
                    in
                    Svg.g []
                        [ Svg.circle [ SA.cx (String.fromFloat x), SA.cy (String.fromFloat y), SA.r "15", SA.class "g-node", SA.style ("fill:" ++ colorOf i) ] []
                        , Svg.text_ [ SA.x (String.fromFloat x), SA.y (String.fromFloat (y + 5)), SA.class "g-label" ] [ Svg.text label ]
                        ]
                )
                g.labels
    in
    Svg.svg [ SA.viewBox ("0 0 " ++ String.fromInt size ++ " " ++ String.fromInt size), SA.class "graph-svg" ] (edges ++ nodes)
