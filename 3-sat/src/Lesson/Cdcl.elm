module Lesson.Cdcl exposing (Model, Msg, example, init, subscriptions, update, view)

import Html exposing (Html, div, li, ol, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import ImplicationGraph
import Random
import Sat.Cdcl as C
import Sat.Dpll as D
import Sat.Formula exposing (Clause, Formula)
import Sat.Generate as G
import Sat.Trail as Trail
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


{-| A hand-made opening formula. With decisions x₁, x₂, x₃, x₄ (all true, in
order) the first conflict happens at level 4 and takes three resolutions; its
1UIP is the forced x₅ rather than the decision x₄, and the backjump skips the
irrelevant levels 2 and 3. The last two clauses only make x₂, x₃ matter.
-}
example : Formula
example =
    { numVars = 8
    , clauses =
        [ [ -4, 5 ]
        , [ -5, -1, 6 ]
        , [ -5, 7 ]
        , [ -6, -7, 8 ]
        , [ -6, -8 ]
        , [ -1, 2, 3 ]
        , [ -2, 3, 4 ]
        ]
    }


init : Random.Seed -> Model
init seed =
    fresh example seed


fresh : Formula -> Random.Seed -> Model
fresh formula seed =
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
                        C.ConflictOn _ ->
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
            let
                ( formula, seed ) =
                    Random.step (G.formula n 52 3) m.seed
            in
            fresh formula seed


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

                C.Resolved r ->
                    Just r.reason

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
            , t " keeps DPLL's loop but changes what happens at a conflict. Instead of just undoing the last decision, it works out "
            , em "which"
            , t " assignments caused the conflict and records that as a new clause."
            ]
        , p
            [ t "To do that, every literal on the trail remembers why it is there: it is either a decision, or it was forced by a clause, its "
            , em "reason"
            , t ". If (¬x₁ ∨ ¬x₂ ∨ x₃) forced x₃, it did so because x₁ and x₂ were already true, so draw arrows x₁ → x₃ and x₂ → x₃. Doing this for every forced literal gives the "
            , strong "implication graph"
            , t ", a "
            , wiki "Directed acyclic graph" "DAG"
            , t " whose sources are decisions. A conflict adds a node κ with arrows from the assignments that made the conflict clause false. Conflict analysis then walks this graph backwards from κ:"
            ]
        , ol [ class "steps" ]
            [ li []
                [ strong "Start with the conflict clause."
                , t " All its literals are false. Read it as a set of nodes that "
                , em "together"
                , t " lead to κ: a cut through the graph that separates κ from the decisions. The clause says that not all of these assignments can hold at once."
                ]
            , li []
                [ strong "Look at the literals from the current level"
                , t ", the level of the newest decision. If there are two or more, take the one assigned most recently. It cannot be the decision, which is the oldest assignment at its level, so it was forced. Replace it by the other literals of its reason. This is "
                , strong "resolution"
                , t ": from (A ∨ x) and (B ∨ ¬x) derive (A ∨ B). The new clause is still all false and still leads to κ. The cut has moved one step back towards the decisions."
                ]
            , li []
                [ strong "Stop when exactly one current-level literal is left."
                , t " Why one? Undo the current level and every other literal of the clause is still false, so the clause is unit: it immediately forces something new. With two or more, it would just be satisfiable again and teach the solver nothing right now. The node where this first happens is the "
                , strong "first unique implication point"
                , t " (1UIP). Every path from the current decision to κ passes through it: it is a "
                , wiki "Dominator (graph theory)" "dominator"
                , t " of the conflict. The walk always ends, because at worst the decision itself is the one node left."
                ]
            , li []
                [ strong "Learn the clause and "
                , Html.strong [] [ wiki "Backjumping" "backjump" ]
                , t " to the second-highest level among its literals (level 0 if it has only one). At that level all its literals except the UIP's are still false, so it is unit and forces the UIP's negation. The levels skipped in between had nothing to do with the conflict, so nothing is lost by undoing them."
                ]
            ]
        , p [ t "Step through the example below. Once a conflict happens, each Step performs one resolution, and the graph shows the clause being analysed moving back from κ." ]
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
        , analysisView s
        , graphView s
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
        , Ui.row [ Ui.btn "Compare with DPLL" Compare, Ui.btn "New random formula" New ]
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


graphView : C.State -> Html msg
graphView s =
    let
        plain =
            { trail = s.trail, level = s.level, conflict = Nothing, frontier = [], resolved = [], uip = Nothing }

        cfg =
            case ( s.analysis, s.event ) of
                ( Just pending, _ ) ->
                    { plain
                        | conflict = Just pending.conflict
                        , frontier = pending.current
                        , resolved = List.map .pivot pending.resolutions
                    }

                ( Nothing, C.Learned an ) ->
                    { trail = an.trailBefore
                    , level = an.fromLevel
                    , conflict = Just an.conflict
                    , frontier = an.learned
                    , resolved = List.map .pivot an.resolutions
                    , uip = Just an.uip
                    }

                _ ->
                    plain
    in
    if List.isEmpty cfg.trail then
        text ""

    else
        div []
            [ Ui.h3 "Implication graph"
            , ImplicationGraph.view cfg
            , case s.event of
                C.Learned _ ->
                    Html.p [ class "caption" ] [ text "The graph as it was at the conflict, before the backjump. The trail below is already after it." ]

                _ ->
                    text ""
            ]


{-| The clause, with its literals from `level` underlined.
-}
clauseAt : List Trail.Entry -> Int -> Clause -> Html msg
clauseAt trail level c =
    if List.isEmpty c then
        text (Ui.clauseText c)

    else
        span [ class "mono" ]
            (text "("
                :: List.intersperse (text " ∨ ")
                    (List.map
                        (\l ->
                            if Trail.levelOf trail l == Just level then
                                span [ class "res-cur" ] [ text (Ui.litName l) ]

                            else
                                text (Ui.litName l)
                        )
                        c
                    )
                ++ [ text ")" ]
            )


atLevel : List Trail.Entry -> Int -> Clause -> List Int
atLevel trail level =
    List.filter (\l -> Trail.levelOf trail l == Just level)


analysisView : C.State -> Html msg
analysisView s =
    let
        panel trail level conflict resolutions footer =
            div [ class "panel analysis" ]
                (Ui.h3 "Conflict analysis"
                    :: div [ class "res-row" ] [ span [ class "res-label" ] [ text "conflict" ], clauseAt trail level conflict ]
                    :: List.map
                        (\r ->
                            div [ class "res-step" ]
                                [ div [ class "res-row" ] [ span [ class "res-label" ] [ text ("resolve on " ++ Ui.varName r.pivot ++ " with") ], span [ class "mono" ] [ text (Ui.clauseText r.reason) ] ]
                                , div [ class "res-row" ] [ span [ class "res-label" ] [ text "→" ], clauseAt trail level r.resolvent ]
                                ]
                        )
                        resolutions
                    ++ footer
                )
    in
    case ( s.analysis, s.event ) of
        ( Just pending, _ ) ->
            let
                current =
                    atLevel s.trail s.level pending.current

                k =
                    List.length current
            in
            panel s.trail
                s.level
                pending.conflict
                (List.reverse pending.resolutions)
                [ p
                    [ t ("Underlined: literals from the current level " ++ String.fromInt s.level ++ ". There ")
                    , t
                        (if k == 1 then
                            "is only one, " ++ String.concat (List.map Ui.litName current) ++ ", so this clause is the one to learn. Next step: learn it and backjump."

                         else
                            "are " ++ String.fromInt k ++ ", so keep going: next step resolves away the newest of them."
                        )
                    ]
                ]

        ( Nothing, C.Learned an ) ->
            panel an.trailBefore
                an.fromLevel
                an.conflict
                an.resolutions
                [ div [ class "res-row learned" ] [ span [ class "res-label" ] [ text "learn" ], span [ class "mono" ] [ text (Ui.clauseText an.learned) ] ]
                , p
                    [ t ("Only " ++ Ui.litName an.uip ++ " comes from level " ++ String.fromInt an.fromLevel ++ " (the 1UIP). Backjump from level " ++ String.fromInt an.fromLevel ++ " to level " ++ String.fromInt an.backjumpTo ++ "; there the learned clause is unit and forces " ++ Ui.litName an.uip ++ ".")
                    ]
                ]

        _ ->
            text ""


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
            text ("Conflict: " ++ Ui.clauseText c ++ " is all false. Next steps: analyse it.")

        C.Resolved r ->
            text (Ui.varName r.pivot ++ " was the newest current-level assignment in the clause; replaced it by the rest of its reason " ++ Ui.clauseText r.reason ++ ".")

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
