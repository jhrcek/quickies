module Lesson.Cdcl exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, div, li, ol, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Random
import Sat.Cdcl as C
import Sat.Dpll as D
import Sat.Generate as G
import Time
import TrailView
import Ui exposing (em, insight, p, strong, t, wiki)
import Undo


type alias Comparison =
    { dpll : D.Stats, cdcl : C.Stats, sat : Bool }


type alias Model =
    { state : C.State
    , past : List C.State
    , playing : Bool
    , seed : Random.Seed
    , comparison : Maybe Comparison
    }


type Msg
    = Step
    | Back
    | Play
    | Pause
    | Tick
    | Reset
    | Finish
    | Compare
    | New
    | ToNextConflict


n : Int
n =
    12


init : Random.Seed -> Model
init seed0 =
    let
        ( formula, seed ) =
            Random.step (G.formula n 52 3) seed0
    in
    { state = C.init formula, past = [], playing = False, seed = seed, comparison = Nothing }


update : Msg -> Model -> Model
update msg m =
    case msg of
        Step ->
            Undo.advance C.step m

        Back ->
            Undo.back { m | playing = False }

        ToNextConflict ->
            let
                stop s =
                    case s.event of
                        C.Learned _ ->
                            True

                        _ ->
                            s.status /= C.Running
            in
            Undo.advanceUntil stop C.step { m | playing = False }

        Tick ->
            if m.state.status == C.Running then
                Undo.advance C.step m

            else
                { m | playing = False }

        Play ->
            { m | playing = True }

        Pause ->
            { m | playing = False }

        Reset ->
            { m | state = C.init m.state.formula, past = [], playing = False }

        Finish ->
            Undo.advanceUntil (\s -> s.status /= C.Running) C.step { m | playing = False }

        Compare ->
            let
                d =
                    D.solve m.state.formula

                c =
                    C.solve m.state.formula
            in
            { m
                | comparison =
                    Just
                        { dpll = d.stats
                        , cdcl = c.stats
                        , sat =
                            case c.status of
                                C.Sat _ ->
                                    True

                                _ ->
                                    False
                        }
            }

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
            C.assignment s

        focus =
            case s.event of
                C.Propagated _ c ->
                    Just c

                C.ConflictOn c ->
                    Just c

                _ ->
                    Nothing

        mark c =
            if Just c == focus then
                Ui.Focus

            else
                Ui.NoMark

        learnedMark c =
            if Just c == focus then
                Ui.LearnedFocus

            else
                Ui.Learned
    in
    div []
        [ p
            [ strong "Conflict-Driven Clause Learning"
            , t " keeps DPLL's loop but changes what happens at a conflict. Every forced literal remembers the clause that forced it (its "
            , em "reason"
            , t "). These reasons form the "
            , strong "implication graph"
            , t ", a DAG whose sources are decisions, so a conflict can be traced back to its causes:"
            ]
        , ol [ class "steps" ]
            [ li [] [ t "Start from the false clause. Its literals are false because of earlier assignments." ]
            , li [] [ t "Pick the most recently forced literal of the current level and replace it by the reasons that forced it. Formally this is ", strong "resolution", t ": from (A ∨ x) and (B ∨ ¬x) derive (A ∨ B)." ]
            , li [] [ t "Repeat until only one literal from the current level is left — the ", strong "first unique implication point", t " (1UIP). In graph terms it is the ", wiki "Dominator (graph theory)" "dominator", t " of the conflict closest to it: every path from the current decision to the conflict passes through it." ]
            , li [] [ t "Learn that clause. Then ", Html.strong [] [ wiki "Backjumping" "backjump" ], t " to the second-highest level in it, possibly skipping many levels. The learned clause is now unit and flips the UIP automatically." ]
            ]
        , Ui.playControls
            { running = s.status == C.Running
            , playing = m.playing
            , canBack = Undo.canBack m
            , onBack = Back
            , onStep = Step
            , onPlay = Play
            , onPause = Pause
            , onReset = Reset
            , onFinish = Finish
            }
        , Ui.row [ Ui.btnIf (s.status == C.Running) "Run to next conflict" ToNextConflict ]
        , Ui.eventLine [ eventText s ]
        , statusBanner s
        , case s.event of
            C.Learned an ->
                analysisView an

            _ ->
                text ""
        , TrailView.view s.trail
        , Ui.stats
            [ Ui.stat "decisions" (String.fromInt s.stats.decisions)
            , Ui.stat "conflicts" (String.fromInt s.stats.conflicts)
            , Ui.stat "learned" (String.fromInt (List.length s.learned))
            , Ui.stat "longest jump" (String.fromInt s.stats.maxJump ++ " lvl")
            ]
        , if List.isEmpty s.learned then
            text ""

          else
            let
                newest =
                    List.take 8 s.learned

                -- Also show the focused learned clause if it is older than the newest 8.
                shownLearned =
                    case focus of
                        Just c ->
                            if List.member c s.learned && not (List.member c newest) then
                                newest ++ [ c ]

                            else
                                newest

                        Nothing ->
                            newest
            in
            div []
                [ Ui.h3 "Learned clauses (newest first)"
                , Ui.viewClauses a learnedMark shownLearned
                ]
        , Ui.h3 "Original clauses"
        , Ui.viewClauses a mark s.formula.clauses
        , Ui.row [ Ui.btn "Compare with DPLL" Compare, Ui.btn "New formula" New ]
        , case m.comparison of
            Just cmp ->
                comparisonView cmp

            Nothing ->
                text ""
        , insight "Why learning is so powerful"
            [ p
                [ t "A learned clause is a lemma: it follows from the formula (it was derived by resolution), so adding it never changes the answer. But it permanently forbids "
                , em "every"
                , t " assignment containing the same root cause, in every future branch. DPLL keeps re-discovering the same contradiction; CDCL hits it once."
                ]
            , p [ t "On a random formula of 12 variables the gap is modest. On industrial formulas — with structure, where the same few causes reappear constantly — learning is the difference between seconds and the age of the universe. It is also ", wiki "Proof complexity" "proof-theoretically stronger", t ": with restarts, CDCL can simulate general resolution, while DPLL is stuck with tree-like resolution." ]
            ]
        ]


analysisView : C.Analysis -> Html msg
analysisView an =
    div [ class "panel analysis" ]
        [ Ui.h3 "Conflict analysis"
        , div [ class "res-row" ] [ span [ class "res-label" ] [ text "conflict" ], span [ class "mono" ] [ text (Ui.clauseText an.conflict) ] ]
        , div []
            (List.map
                (\r ->
                    div [ class "res-step" ]
                        [ div [ class "res-row" ] [ span [ class "res-label" ] [ text ("resolve on " ++ Ui.varName r.pivot ++ " with") ], span [ class "mono" ] [ text (Ui.clauseText r.reason) ] ]
                        , div [ class "res-row" ] [ span [ class "res-label" ] [ text "→" ], span [ class "mono" ] [ text (Ui.clauseText r.resolvent) ] ]
                        ]
                )
                an.resolutions
            )
        , div [ class "res-row learned" ] [ span [ class "res-label" ] [ text "learn" ], span [ class "mono" ] [ text (Ui.clauseText an.learned) ] ]
        , p
            [ t ("Only " ++ Ui.litName an.uip ++ " comes from level " ++ String.fromInt an.fromLevel ++ " (the 1UIP). Backjump from level " ++ String.fromInt an.fromLevel ++ " to level " ++ String.fromInt an.backjumpTo ++ "; there the learned clause is unit and forces " ++ Ui.litName an.uip ++ ".")
            ]
        ]


comparisonView : Comparison -> Html msg
comparisonView c =
    div [ class "panel" ]
        [ p
            [ t
                ("Both solvers, run to the end on this formula ("
                    ++ (if c.sat then
                            "satisfiable"

                        else
                            "unsatisfiable"
                       )
                    ++ "):"
                )
            ]
        , table [ class "tt num" ]
            [ thead [] [ tr [] [ th [] [], th [] [ text "DPLL" ], th [] [ text "CDCL" ] ] ]
            , tbody []
                [ tr [] [ td [] [ text "decisions" ], td [] [ text (String.fromInt c.dpll.decisions) ], td [] [ text (String.fromInt c.cdcl.decisions) ] ]
                , tr [] [ td [] [ text "conflicts" ], td [] [ text (String.fromInt c.dpll.conflicts) ], td [] [ text (String.fromInt c.cdcl.conflicts) ] ]
                , tr [] [ td [] [ text "propagations" ], td [] [ text (String.fromInt c.dpll.propagations) ], td [] [ text (String.fromInt c.cdcl.propagations) ] ]
                ]
            ]
        ]


eventText : C.State -> Html msg
eventText s =
    case s.event of
        C.Started ->
            text "Press Step to begin, or jump straight to the first conflict."

        C.Decided l ->
            text ("Decide " ++ Ui.litName l ++ " at level " ++ String.fromInt s.level ++ ".")

        C.Propagated l c ->
            text (Ui.litName l ++ " is forced by " ++ Ui.clauseText c ++ ".")

        C.ConflictOn c ->
            text ("Conflict: " ++ Ui.clauseText c ++ " is all false. Next step: analyse it.")

        C.Learned an ->
            text ("Learned " ++ Ui.clauseText an.learned ++ ", jumped back to level " ++ String.fromInt an.backjumpTo ++ ".")

        C.FoundModel ->
            text "All original clauses satisfied."

        C.ProvedUnsat c ->
            text ("Conflict on " ++ Ui.clauseText c ++ " at level 0, with no decisions to blame.")


statusBanner : C.State -> Html msg
statusBanner s =
    case s.status of
        C.Sat _ ->
            Ui.banner "good" [ strong "SAT. ", text "The trail satisfies every original clause." ]

        C.Unsat ->
            Ui.banner "bad" [ strong "UNSAT. ", text "A conflict that needs no decisions means the formula itself is contradictory. The learned clauses form the proof." ]

        C.Running ->
            text ""
