module Lesson.LocalSearch exposing (Model, Msg, init, subscriptions, update, view)

import Dict
import Html exposing (Html, div, input, label, span, text)
import Html.Attributes as HA exposing (class, classList)
import Html.Events exposing (onInput)
import Random
import Sat.Generate as G
import Sat.WalkSat as W
import Svg
import Svg.Attributes as SA
import Time
import Ui exposing (em, insight, p, strong, t)


type alias Model =
    { state : W.State
    , noise : Float
    , playing : Bool
    , seed : Random.Seed
    }


type Msg
    = Step
    | Play
    | Pause
    | Tick
    | Restart
    | New
    | SetNoise String


n : Int
n =
    40


m_ : Int
m_ =
    168


maxFlips : Int
maxFlips =
    5000


init : Random.Seed -> Model
init seed0 =
    let
        ( ( formula, _ ), seed1 ) =
            Random.step (G.planted n m_ 3) seed0

        ( s, seed2 ) =
            Random.step Random.independentSeed seed1
    in
    { state = W.init s formula, noise = 0.5, playing = False, seed = seed2 }


update : Msg -> Model -> Model
update msg m =
    case msg of
        Step ->
            { m | state = W.step m.noise maxFlips m.state }

        Tick ->
            if m.state.status == W.Searching then
                { m | state = W.step m.noise maxFlips (W.step m.noise maxFlips m.state) }

            else
                { m | playing = False }

        Play ->
            { m | playing = True }

        Pause ->
            { m | playing = False }

        Restart ->
            let
                ( s, seed ) =
                    Random.step Random.independentSeed m.seed
            in
            { m | state = W.init s m.state.formula, seed = seed, playing = False }

        New ->
            let
                fresh =
                    init m.seed
            in
            { fresh | noise = m.noise }

        SetNoise str ->
            { m | noise = String.toFloat str |> Maybe.withDefault m.noise }


subscriptions : Model -> Sub Msg
subscriptions m =
    if m.playing then
        Time.every 60 (\_ -> Tick)

    else
        Sub.none


view : Model -> Html Msg
view m =
    let
        s =
            m.state

        unsat =
            W.unsatisfied s.formula s.assignment

        lastVar =
            Maybe.map .var s.last
    in
    div []
        [ p
            [ t "Now something completely different. Forget trees and trails: start with a "
            , em "random complete"
            , t " assignment and repair it. This is what you did by hand in the Formulas step, made systematic by "
            , strong "WalkSAT"
            , t ":"
            ]
        , Html.ol [ class "steps" ]
            [ Html.li [] [ t "Pick a random unsatisfied clause." ]
            , Html.li [] [ t "Flipping any of its 3 variables fixes it, but may ", em "break", t " clauses that are currently satisfied. Count the breaks for each." ]
            , Html.li [] [ t "If some flip breaks nothing, take it. Otherwise, with probability ", em "p", t " (the noise) flip a random one, else the one that breaks the fewest." ]
            ]
        , p [ t ("Below: a satisfiable formula with " ++ String.fromInt n ++ " variables and " ++ String.fromInt m_ ++ " clauses (ratio 4.2). Each square is a variable; the outlined one was flipped last.") ]
        , Ui.row
            [ Ui.btnIf (s.status == W.Searching) "Flip" Step
            , if m.playing then
                Ui.btnPrimary "Pause" Pause

              else
                Ui.btnPrimary "Run" Play
            , Ui.btn "Restart" Restart
            , Ui.btn "New formula" New
            ]
        , label [ class "slider" ]
            [ span [] [ text ("noise p = " ++ String.fromFloat m.noise) ]
            , input [ HA.type_ "range", HA.min "0", HA.max "1", HA.step "0.05", HA.value (String.fromFloat m.noise), onInput SetNoise ] []
            ]
        , div [ class "vargrid" ]
            (Dict.toList s.assignment
                |> List.map
                    (\( v, b ) ->
                        span
                            [ class
                                (if b then
                                    "vcell is-t"

                                 else
                                    "vcell is-f"
                                )
                            , classList [ ( "flipped", lastVar == Just v ) ]
                            ]
                            [ text (String.fromInt v) ]
                    )
            )
        , Ui.stats
            [ Ui.stat "unsatisfied now" (String.fromInt (List.length unsat))
            , Ui.stat "best so far" (String.fromInt s.best)
            , Ui.stat "flips" (String.fromInt s.flips)
            ]
        , div [ class "caption" ] [ text ("Unsatisfied clauses over the last " ++ String.fromInt (List.length s.history) ++ " flips (top = " ++ String.fromInt (List.maximum s.history |> Maybe.withDefault 0) ++ ")") ]
        , div [ class "fig spark" ] [ sparkline (List.reverse s.history) ]
        , case s.last of
            Just fl ->
                Ui.eventLine [ text ("Clause " ++ Ui.clauseText fl.clause ++ " was unsatisfied; flipped " ++ Ui.varName fl.var ++ " (" ++ fl.how ++ ").") ]

            Nothing ->
                Ui.eventLine [ text "Random start. Press Flip or Run." ]
        , case s.status of
            W.Solved ->
                Ui.banner "good" [ strong "Solved ", text ("after " ++ String.fromInt s.flips ++ " flips.") ]

            W.GaveUp ->
                Ui.banner "bad" [ strong "Gave up ", text ("after " ++ String.fromInt maxFlips ++ " flips. Restart and try again, or change the noise.") ]

            W.Searching ->
                text ""
        , Ui.details ("Show the " ++ String.fromInt (List.length unsat) ++ " unsatisfied clauses")
            [ Ui.viewClauses s.assignment (always Ui.NoMark) unsat ]
        , insight "Try the extremes"
            [ p [ t "Set p = 0: pure greed. It plunges quickly, then gets stuck in a local minimum, cycling between a few near-solutions. Set p = 1: a pure random walk, which wanders aimlessly. Somewhere around 0.4–0.6 the walk is greedy enough to make progress and random enough to escape traps." ]
            ]
        , insight "A random walk that beats 2ⁿ"
            [ p
                [ t "Schöning (1999) analysed the pure version: random start, flip a random variable of a random violated clause, restart after 3n flips. Fix any solution S. The chosen clause is violated now but satisfied by S, so at least one of its three variables disagrees with S: with probability at least 1/3 the flip moves you one step closer to S. A gambler's-ruin argument then shows the expected running time is about (4/3)ⁿ — exponentially faster than 2ⁿ, and still one of the best known bounds."
                ]
            , p [ t "The catch: local search is incomplete. It can find a solution, but it can never tell you there is none. For proofs of unsatisfiability you need the systematic solvers from before." ]
            ]
        ]


sparkline : List Int -> Html msg
sparkline hist =
    let
        w =
            340

        h =
            90

        maxV =
            hist |> List.maximum |> Maybe.withDefault 1 |> max 1

        count =
            max 2 (List.length hist)

        pts =
            hist
                |> List.indexedMap
                    (\i v ->
                        String.fromFloat (toFloat i / toFloat (count - 1) * w)
                            ++ ","
                            ++ String.fromFloat (h - 6 - toFloat v / toFloat maxV * (h - 16))
                    )
                |> String.join " "
    in
    Svg.svg [ SA.viewBox ("0 0 " ++ String.fromInt w ++ " " ++ String.fromInt h), SA.class "spark-svg", SA.preserveAspectRatio "none" ]
        [ Svg.line [ SA.x1 "0", SA.x2 (String.fromInt w), SA.y1 (String.fromInt (h - 6)), SA.y2 (String.fromInt (h - 6)), SA.class "axis" ] []
        , Svg.polyline [ SA.points pts, SA.class "spark-line" ] []
        ]
