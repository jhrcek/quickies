module Lesson.UnitPropagation exposing (Model, Msg, init, update, view)

import Dict
import Html exposing (Html, div, li, ol, span, text)
import Html.Attributes exposing (class)
import Random
import Sat.Formula as F exposing (Assignment, Clause, Formula)
import Sat.Generate as G
import Sat.Propagate as P exposing (Implication)
import Ui exposing (em, insight, p, strong, t)


type alias Model =
    { formula : Formula
    , decisions : Assignment
    , implied : List Implication -- oldest first
    , conflict : Maybe Clause
    , seed : Random.Seed
    }


type Msg
    = Cycle Int
    | PropagateOne
    | PropagateAll
    | Clear
    | New


n : Int
n =
    8


init : Random.Seed -> Model
init seed0 =
    let
        ( ( formula, _ ), seed ) =
            Random.step (G.planted n 30 3) seed0
    in
    { formula = formula, decisions = Dict.empty, implied = [], conflict = Nothing, seed = seed }


current : Model -> Assignment
current m =
    List.foldl (\i -> F.assign i.lit) m.decisions m.implied


update : Msg -> Model -> Model
update msg m =
    case msg of
        Cycle v ->
            let
                a =
                    current m

                next =
                    case Dict.get v a of
                        Nothing ->
                            Just True

                        Just True ->
                            Just False

                        Just False ->
                            Nothing
            in
            { m
                | decisions =
                    case next of
                        Just b ->
                            Dict.insert v b m.decisions

                        Nothing ->
                            Dict.remove v m.decisions
                , implied = []
                , conflict = Nothing
            }

        PropagateOne ->
            if m.conflict /= Nothing then
                m

            else
                case P.once m.formula.clauses (current m) of
                    P.Implied i ->
                        { m | implied = m.implied ++ [ i ] }

                    P.Conflict c ->
                        { m | conflict = Just c }

                    P.Quiescent ->
                        m

        PropagateAll ->
            case P.toFixpoint m.formula.clauses (current m) of
                P.Stable _ is ->
                    { m | implied = m.implied ++ is }

                P.ConflictAt is c ->
                    { m | implied = m.implied ++ is, conflict = Just c }

        Clear ->
            { m | decisions = Dict.empty, implied = [], conflict = Nothing }

        New ->
            init m.seed


view : Model -> Html Msg
view m =
    let
        a =
            current m

        lastReason =
            List.reverse m.implied |> List.head |> Maybe.map .reason

        mark c =
            if Just c == m.conflict || Just c == lastReason then
                Ui.Focus

            else
                Ui.NoMark

        quiet =
            P.once m.formula.clauses a == P.Quiescent
    in
    div []
        [ p
            [ t "Look at (¬x₁ ∨ x₂ ∨ x₃) after setting x₁ = T and x₂ = F. Two of its literals are false, so the clause now says only one thing: "
            , strong "x₃ must be true"
            , t ". A clause with every literal false but one unassigned is a "
            , strong "unit clause"
            , t ", and it forces its last literal — the implication reading from the Clauses step in action. Setting that literal can make other clauses unit, and the forced moves cascade. Repeating this to a fixpoint is called "
            , strong "unit propagation"
            , t " or Boolean constraint propagation (BCP)."
            ]
        , p [ t "Tap switches to make decisions (? → T → F → ?). Amber clauses are unit. Then propagate, one step at a time or all at once." ]
        , Ui.varToggles n a (state m) Cycle
        , Ui.row
            [ Ui.btnIf (not quiet && m.conflict == Nothing) "Propagate one" PropagateOne
            , Ui.btnIf (not quiet && m.conflict == Nothing) "Propagate all" PropagateAll
            , Ui.btn "Clear" Clear
            , Ui.btn "New formula" New
            ]
        , case m.conflict of
            Just c ->
                Ui.banner "bad"
                    [ strong "Conflict. "
                    , t ("Clause " ++ Ui.clauseText c ++ " has every literal false. Your decisions cannot all be part of a solution — a solver would now backtrack.")
                    ]

            Nothing ->
                if Dict.isEmpty m.decisions then
                    text ""

                else if quiet then
                    Ui.eventLine [ t "Nothing left to propagate. To go further, someone has to make another decision." ]

                else
                    let
                        units =
                            List.filter
                                (\c ->
                                    case F.clauseStatus a c of
                                        F.Unit _ ->
                                            True

                                        _ ->
                                            False
                                )
                                m.formula.clauses
                    in
                    Ui.eventLine [ t (String.fromInt (List.length units) ++ " unit clause(s) waiting.") ]
        , if List.isEmpty m.implied then
            text ""

          else
            ol [ class "log" ]
                (List.map
                    (\i ->
                        li []
                            [ span [ class "lit is-t" ] [ text (Ui.litName i.lit) ]
                            , span [ class "because" ] [ text (" forced by " ++ Ui.clauseText i.reason) ]
                            ]
                    )
                    m.implied
                )
        , Ui.stats
            [ Ui.stat "decisions" (String.fromInt (Dict.size m.decisions))
            , Ui.stat "forced" (String.fromInt (List.length m.implied))
            , Ui.stat "still open" (String.fromInt (n - Dict.size a))
            ]
        , Ui.viewClauses a mark m.formula.clauses
        , insight "Why 2-SAT is easy and 3-SAT is not"
            [ p
                [ t "With two literals per clause, (a ∨ b) is the pair of implications ¬a → b and ¬b → a: one false literal immediately forces the other. Build a graph from those arrows and the formula is unsatisfiable exactly when some x and ¬x reach each other — checkable in linear time (Aspvall, Plass and Tarjan, 1979)."
                ]
            , p
                [ t "A 3-clause stays silent until "
                , em "two"
                , t " of its literals are false. Propagation alone gets stuck, and somebody has to guess. That one extra literal is the gap between linear time and NP-completeness."
                ]
            , p [ t "2-SAT is not the only island of tractability. For Horn formulas (at most one positive literal per clause) unit propagation alone is a complete decision procedure: propagate from the empty assignment, and if there is no conflict, setting every remaining variable to F gives a model. Schaefer's dichotomy theorem (1978) says this is the whole story for Boolean constraint satisfaction: a constraint language is polynomial if it falls into one of six classes (Horn, dual-Horn, 2-CNF, affine, 0-valid, 1-valid) and NP-complete otherwise." ]
            ]
        , insight "Making it fast: watched literals"
            [ p [ t "Real solvers spend most of their time propagating, so they avoid scanning every clause. Each clause watches just two of its unfalsified literals; a clause is only revisited when one of its watches becomes false. As long as both watched literals are not false, the clause cannot be unit or conflicting. Backtracking then costs nothing: the watches stay valid." ]
            ]
        ]


state : Model -> Int -> Ui.ToggleState
state m v =
    case List.filter (\i -> abs i.lit == v) m.implied of
        i :: _ ->
            Ui.Locked ("forced by " ++ Ui.clauseText i.reason)

        [] ->
            Ui.Free
