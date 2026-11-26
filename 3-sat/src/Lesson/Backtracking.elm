module Lesson.Backtracking exposing (Model, Msg, init, subscriptions, update, view)

import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Random
import Sat.Backtrack as B exposing (Node, Outcome(..))
import Sat.Formula exposing (Formula)
import Sat.Generate as G
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time
import Ui exposing (em, insight, p, strong, t, wiki)


type alias Model =
    { formula : Formula
    , nodes : List Node
    , shown : Int
    , playing : Bool
    , seed : Random.Seed
    }


type Msg
    = Step
    | Play
    | Pause
    | Tick
    | Reset
    | Finish
    | New


n : Int
n =
    4


init : Random.Seed -> Model
init seed0 =
    let
        ( formula, seed ) =
            Random.step (G.formula n 10 3) seed0
    in
    { formula = formula, nodes = B.explore formula, shown = 1, playing = False, seed = seed }


update : Msg -> Model -> Model
update msg m =
    let
        total =
            List.length m.nodes
    in
    case msg of
        Step ->
            { m | shown = min total (m.shown + 1) }

        Tick ->
            if m.shown >= total then
                { m | playing = False }

            else
                { m | shown = m.shown + 1 }

        Play ->
            { m | playing = True }

        Pause ->
            { m | playing = False }

        Reset ->
            { m | shown = 1, playing = False }

        Finish ->
            { m | shown = total, playing = False }

        New ->
            init m.seed


subscriptions : Model -> Sub Msg
subscriptions m =
    if m.playing then
        Time.every 350 (\_ -> Tick)

    else
        Sub.none


view : Model -> Html Msg
view m =
    let
        visible =
            List.take m.shown m.nodes

        current =
            List.reverse visible |> List.head

        solutions =
            List.filter (\x -> x.outcome == Solution) visible |> List.length

        pruned =
            List.filter (\x -> x.outcome /= Inner && x.outcome /= Solution) visible |> List.length

        total =
            List.length m.nodes
    in
    div []
        [ p
            [ t "Brute force fills in every variable before looking at a single clause. Smarter: assign variables one at a time and check the clauses "
            , em "as you go"
            , t ". The moment some clause has all three literals false, no completion of this partial assignment can work — so abandon the whole branch. This is "
            , wiki "Backtracking" "backtracking"
            , t "."
            ]
        , p [ t "Below is the search tree for a 4-variable formula. Going left sets the variable to T, right sets it to F. The faint tree is everything brute force would visit." ]
        , Ui.playControls
            { running = m.shown < total
            , playing = m.playing
            , onStep = Step
            , onPlay = Play
            , onPause = Pause
            , onReset = Reset
            , onFinish = Finish
            }
        , div [ class "fig tree" ] [ treeSvg visible current ]
        , case current of
            Just node ->
                Ui.eventLine [ nodeText node ]

            Nothing ->
                text ""
        , Ui.stats
            [ Ui.stat "nodes visited" (String.fromInt m.shown ++ " / 31")
            , Ui.stat "branches pruned" (String.fromInt pruned)
            , Ui.stat "solutions" (String.fromInt solutions)
            ]
        , Ui.viewClauses (Maybe.map (.path >> B.pathAssignment) current |> Maybe.withDefault Dict.empty) (mark current) m.formula.clauses
        , Ui.row [ Ui.btn "New formula" New ]
        , insight "One early 'no' saves a whole subtree"
            [ p
                [ t "Pruning a node at depth d skips everything below it: 2^(n−d+1) − 2 nodes. Near the root that is almost half the tree. This is why "
                , strong "the order"
                , t " in which variables are tried matters so much — real solvers choose the next variable dynamically, aiming to hit contradictions (or solutions) as early as possible."
                ]
            , p [ t "We explore the full tree here to show every solution. A solver that only needs one would stop at the first green leaf." ]
            ]
        ]


mark : Maybe Node -> List Int -> Ui.ClauseMark
mark current c =
    case current of
        Just { outcome } ->
            case outcome of
                Pruned pc ->
                    if pc == c then
                        Ui.Focus

                    else
                        Ui.NoMark

                _ ->
                    Ui.NoMark

        Nothing ->
            Ui.NoMark


nodeText : Node -> Html msg
nodeText node =
    let
        prefix =
            if List.isEmpty node.path then
                "Start with nothing assigned."

            else
                let
                    path =
                        node.path
                            |> List.indexedMap
                                (\i b ->
                                    Ui.varName (i + 1)
                                        ++ "="
                                        ++ (if b then
                                                "T"

                                            else
                                                "F"
                                           )
                                )
                            |> String.join ", "
                in
                path ++ ": "
    in
    case node.outcome of
        Inner ->
            if List.isEmpty node.path then
                text prefix

            else
                text (prefix ++ "no clause is violated yet, go deeper.")

        Pruned c ->
            text (prefix ++ "clause " ++ Ui.clauseText c ++ " is all false. Prune!")

        Solution ->
            text (prefix ++ "every clause satisfied. A solution!")



-- TREE PICTURE


width : Float
width =
    340


levelH : Float
levelH =
    52


pos : List Bool -> ( Float, Float )
pos path =
    let
        d =
            List.length path

        idx =
            List.foldl
                (\b acc ->
                    acc
                        * 2
                        + (if b then
                            0

                           else
                            1
                          )
                )
                0
                path

        slots =
            2 ^ d
    in
    ( 22 + (toFloat idx + 0.5) / toFloat slots * (width - 30), 18 + toFloat d * levelH )


allPaths : Int -> List (List Bool)
allPaths depth =
    let
        go d acc =
            if d <= 0 then
                acc

            else
                go (d - 1) (List.concatMap (\p -> [ p ++ [ True ], p ++ [ False ] ]) acc)
    in
    go depth [ [] ]


treeSvg : List Node -> Maybe Node -> Svg msg
treeSvg visible current =
    let
        ghostPaths =
            List.range 0 n |> List.concatMap allPaths

        edge cls path =
            case List.reverse path of
                [] ->
                    []

                _ :: parentRev ->
                    let
                        ( x1, y1 ) =
                            pos (List.reverse parentRev)

                        ( x2, y2 ) =
                            pos path
                    in
                    [ Svg.line [ SA.x1 (f x1), SA.y1 (f y1), SA.x2 (f x2), SA.y2 (f y2), SA.class cls ] [] ]

        ghost =
            List.concatMap (edge "ghost") ghostPaths
                ++ List.map ghostDot ghostPaths

        drawn =
            List.concatMap (\node -> edge "edge" node.path) visible
                ++ List.map (drawNode current) visible

        labels =
            List.range 1 n
                |> List.map
                    (\d ->
                        Svg.text_ [ SA.x "2", SA.y (f (18 + (toFloat d - 0.5) * levelH + 4)), SA.class "lvl" ] [ Svg.text (Ui.varName d) ]
                    )
    in
    Svg.svg [ SA.viewBox ("0 0 " ++ f width ++ " " ++ f (36 + toFloat n * levelH)), SA.class "tree-svg" ]
        (labels ++ ghost ++ drawn)


ghostDot : List Bool -> Svg msg
ghostDot path =
    let
        ( x, y ) =
            pos path
    in
    Svg.circle [ SA.cx (f x), SA.cy (f y), SA.r "3", SA.class "ghost-dot" ] []


drawNode : Maybe Node -> Node -> Svg msg
drawNode current node =
    let
        ( x, y ) =
            pos node.path

        isCurrent =
            Maybe.map .path current == Just node.path

        ring =
            if isCurrent then
                [ Svg.circle [ SA.cx (f x), SA.cy (f y), SA.r "11", SA.class "ring" ] [] ]

            else
                []
    in
    Svg.g []
        (ring
            ++ (case node.outcome of
                    Inner ->
                        [ Svg.circle [ SA.cx (f x), SA.cy (f y), SA.r "6", SA.class "n-inner" ] [] ]

                    Pruned _ ->
                        [ Svg.circle [ SA.cx (f x), SA.cy (f y), SA.r "7", SA.class "n-pruned" ] []
                        , Svg.text_ [ SA.x (f x), SA.y (f (y + 3.5)), SA.class "n-x" ] [ Svg.text "×" ]
                        ]

                    Solution ->
                        [ Svg.circle [ SA.cx (f x), SA.cy (f y), SA.r "7", SA.class "n-sol" ] [] ]
               )
        )


f : Float -> String
f =
    String.fromFloat
