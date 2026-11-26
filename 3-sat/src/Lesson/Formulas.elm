module Lesson.Formulas exposing (Model, Msg, init, update, view)

import Dict
import Html exposing (Html, div, pre, text)
import Random
import Sat.Formula as F exposing (Assignment, Formula)
import Sat.Generate as G
import Ui exposing (em, insight, p, strong, t, wiki)


type alias Model =
    { formula : Formula
    , assignment : Assignment
    , flips : Int
    , seed : Random.Seed
    }


type Msg
    = Toggle Int
    | NewPuzzle


numVars : Int
numVars =
    5


numClauses : Int
numClauses =
    16


init : Random.Seed -> Model
init seed0 =
    let
        start =
            F.completeWith False numVars Dict.empty

        -- Redraw until the all-False start is not already a solution.
        draw seed =
            let
                ( ( f, _ ), next ) =
                    Random.step (G.planted numVars numClauses 3) seed
            in
            if F.satisfies f start then
                draw next

            else
                ( f, next )

        ( formula, seed1 ) =
            draw seed0
    in
    { formula = formula
    , assignment = start
    , flips = 0
    , seed = seed1
    }


update : Msg -> Model -> Model
update msg m =
    case msg of
        Toggle v ->
            { m | assignment = Dict.update v (Maybe.map not) m.assignment, flips = m.flips + 1 }

        NewPuzzle ->
            init m.seed


view : Model -> Html Msg
view m =
    let
        sat =
            F.countSatisfied m.formula m.assignment

        total =
            List.length m.formula.clauses
    in
    div []
        [ p
            [ t "A formula in "
            , strong "conjunctive normal form"
            , t " (CNF) is a conjunction of clauses, so again it can be treated as a set of clauses; 3-CNF means every clause has three literals. It is "
            , Html.strong [] [ wiki "Satisfiability" "satisfiable" ]
            , t " if some assignment (a "
            , em "model"
            , t ") makes every clause true. In the veto picture, each clause excludes a subcube of {T, F}ⁿ of size 2ⁿ⁻³, and the formula is satisfiable iff these subcubes fail to cover the whole cube."
            ]
        , p
            [ t "Why CNF? Every propositional formula has an equivalent CNF, but distributing ∨ over ∧ can blow it up exponentially. The "
            , Html.strong [] [ wiki "Tseitin transformation" "Tseitin transformation" ]
            , t " avoids that: introduce a fresh variable for every subformula and add a few clauses defining it. The result is not equivalent but "
            , Html.em [] [ wiki "Equisatisfiability" "equisatisfiable" ]
            , t ", and only linearly larger. That is why CNF is the universal input format of SAT solvers."
            ]
        , p [ t "Your turn. This puzzle is guaranteed solvable. Green clauses are satisfied, red ones are not." ]
        , Ui.varToggles numVars m.assignment (always Ui.Free) Toggle
        , Ui.stats
            [ Ui.stat "clauses satisfied" (String.fromInt sat ++ " / " ++ String.fromInt total)
            , Ui.stat "flips so far" (String.fromInt m.flips)
            ]
        , if sat == total then
            Ui.banner "good" [ strong "Satisfied! ", t "Every clause is green. Checking that took one pass over the clauses — keep that in mind." ]

          else
            text ""
        , Ui.viewClauses m.assignment (always Ui.NoMark) m.formula.clauses
        , Ui.row [ Ui.btn "New puzzle" NewPuzzle ]
        , Ui.details "Show it in DIMACS format"
            [ pre [] [ text (F.toDimacs m.formula) ]
            , p [ t "This is the plain-text format every SAT solver reads: a header with variable and clause counts, then one clause per line, ended by 0." ]
            ]
        , insight "Checking is easy, finding is hard"
            [ p
                [ t "Verifying a proposed model takes one linear pass over the clauses. "
                , strong "NP"
                , t " is precisely the class of decision problems whose yes-instances have a polynomial-size certificate checkable in polynomial time; for SAT the certificate is the model itself. Finding one is another matter: you probably fixed one red clause, broke another, and went back and forth."
                ]
            , p
                [ t "What you just did by hand — flip a variable that fixes a red clause, accept that it may break others — is "
                , Html.em [] [ wiki "Local search (optimization)" "local search" ]
                , t ". We will turn it into a real algorithm, "
                , wiki "WalkSAT" "WalkSAT"
                , t ", later on."
                ]
            ]
        ]
