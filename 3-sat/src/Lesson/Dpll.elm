module Lesson.Dpll exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, div, text)
import Random
import Sat.Dpll as D
import Sat.Generate as G
import Time
import TrailView
import Ui exposing (em, insight, p, strong, t)


type alias Model =
    { state : D.State
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
    8


init : Random.Seed -> Model
init seed0 =
    let
        ( formula, seed ) =
            Random.step (G.formula n 36 3) seed0
    in
    { state = D.init formula, playing = False, seed = seed }


update : Msg -> Model -> Model
update msg m =
    case msg of
        Step ->
            { m | state = D.step m.state }

        Tick ->
            if m.state.status == D.Running then
                { m | state = D.step m.state }

            else
                { m | playing = False }

        Play ->
            { m | playing = True }

        Pause ->
            { m | playing = False }

        Reset ->
            { m | state = D.init m.state.formula, playing = False }

        Finish ->
            { m | state = D.solve m.state.formula, playing = False }

        New ->
            init m.seed


subscriptions : Model -> Sub Msg
subscriptions m =
    if m.playing then
        Time.every 300 (\_ -> Tick)

    else
        Sub.none


view : Model -> Html Msg
view m =
    let
        s =
            m.state

        a =
            D.assignment s

        focus =
            case s.event of
                D.Propagated _ c ->
                    Just c

                D.ConflictOn c ->
                    Just c

                _ ->
                    Nothing

        mark c =
            if Just c == focus then
                Ui.Focus

            else
                Ui.NoMark
    in
    div []
        [ p
            [ t "Combine the last two ideas and you get "
            , strong "DPLL"
            , t " (Davis, Putnam, Logemann, Loveland, 1962), the ancestor of every complete SAT solver. The 1960 Davis–Putnam procedure eliminated variables by resolution and ran out of memory; the 1962 version replaced that with splitting, i.e. backtracking search. In outline:"
            ]
        , TrailView.pseudo
            [ "loop:"
            , "  propagate all unit clauses"
            , "  if a clause is false:"
            , "     undo to the last decision not yet flipped"
            , "     none left? → UNSAT; else flip it"
            , "  else if every clause is true → SAT"
            , "  else decide: pick a variable, set it"
            ]
            (activeLine s)
        , p [ t "Each decision opens a new ", em "level", t ". The ", strong "trail", t " below lists every assignment in order, grouped by level: boxed literals are decisions, plain ones were forced by propagation. This formula has 8 variables and 36 clauses — right around the hardest density, as we'll see." ]
        , Ui.playControls
            { running = s.status == D.Running
            , playing = m.playing
            , onStep = Step
            , onPlay = Play
            , onPause = Pause
            , onReset = Reset
            , onFinish = Finish
            }
        , Ui.eventLine [ eventText s ]
        , statusBanner s
        , TrailView.view s.trail
        , Ui.stats
            [ Ui.stat "decisions" (String.fromInt s.stats.decisions)
            , Ui.stat "propagations" (String.fromInt s.stats.propagations)
            , Ui.stat "conflicts" (String.fromInt s.stats.conflicts)
            ]
        , Ui.viewClauses a mark s.formula.clauses
        , Ui.row [ Ui.btn "New formula" New ]
        , insight "The weakness: chronological backtracking"
            [ p
                [ t "When a conflict appears, DPLL blames the "
                , em "most recent"
                , t " decision. But the real culprit may be a decision made ten levels earlier. DPLL then flips decision 10, fails, flips 9, fails, … re-exploring the same doomed territory over and over. It also forgets everything it learned the moment it backtracks."
                ]
            , p [ t "Both problems have one fix: when a conflict happens, work out which decisions actually caused it, and remember that. That is the next step." ]
            , p [ t "(The original DPLL also had a pure-literal rule: a variable occurring with only one sign can be set to satisfy all its clauses. It is omitted here, as in most modern solvers, because it is expensive to maintain and rarely pays off.)" ]
            ]
        , insight "An UNSAT run is a proof"
            [ p [ t "When DPLL answers UNSAT, its search tree is a certificate: every leaf is a falsified clause, every branch covers both values of a variable. Read bottom-up, it is a resolution proof that the formula has no solution. Short proofs ⇔ fast runs, and some formulas (the pigeonhole principle, famously) only have exponentially long resolution proofs — so no DPLL-style solver can be fast on them." ]
            ]
        ]


activeLine : D.State -> Int
activeLine s =
    case s.event of
        D.Started ->
            0

        D.Propagated _ _ ->
            1

        D.ConflictOn _ ->
            2

        D.Backtracked _ ->
            4

        D.ProvedUnsat ->
            4

        D.FoundModel ->
            5

        D.Decided _ ->
            6


eventText : D.State -> Html msg
eventText s =
    case s.event of
        D.Started ->
            text "Press Step to begin."

        D.Decided l ->
            text ("Decide " ++ Ui.litName l ++ " at level " ++ String.fromInt s.level ++ ".")

        D.Propagated l c ->
            text (Ui.litName l ++ " is forced by " ++ Ui.clauseText c ++ ".")

        D.ConflictOn c ->
            text ("Conflict: " ++ Ui.clauseText c ++ " is all false.")

        D.Backtracked l ->
            text ("Undo back to level " ++ String.fromInt s.level ++ " and try the other branch: " ++ Ui.litName l ++ ".")

        D.FoundModel ->
            text "All clauses satisfied."

        D.ProvedUnsat ->
            text "Every branch of every decision failed."


statusBanner : D.State -> Html msg
statusBanner s =
    case s.status of
        D.Sat _ ->
            Ui.banner "good" [ strong "SAT. ", text "The trail is a satisfying assignment (unassigned variables can be anything)." ]

        D.Unsat ->
            Ui.banner "bad" [ strong "UNSAT. ", text "Both branches of every decision led to a conflict, so no assignment works." ]

        D.Running ->
            text ""
