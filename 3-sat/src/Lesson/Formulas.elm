module Lesson.Formulas exposing (Model, Msg, init, update, view)

import Dict
import Html exposing (Html, div, pre, text)
import Random
import Sat.Formula as F exposing (Assignment, Formula)
import Sat.Generate as G
import Ui exposing (em, insight, p, strong, t)


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
        ( ( f, _ ), seed ) =
            Random.step (G.planted numVars numClauses 3) seed0
    in
    { formula = f
    , assignment = F.completeWith False numVars Dict.empty
    , flips = 0
    , seed = seed
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
            [ t "A 3-CNF "
            , strong "formula"
            , t " is an AND of clauses (CNF = conjunctive normal form). It is "
            , strong "satisfiable"
            , t " if one assignment makes every clause true at once. Each clause vetoes one pattern; you need an assignment that dodges all vetoes."
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
                [ t "Verifying a proposed answer takes one pass over the clauses. Problems whose yes-answers can be checked that fast form the class "
                , strong "NP"
                , t ". Finding the answer is another matter: you probably flipped switches, fixed one red clause, broke another, and went back and forth."
                ]
            , p
                [ t "What you just did by hand — flip a variable that fixes a red clause, accept that it may break others — is "
                , em "local search"
                , t ". We will turn it into a real algorithm, WalkSAT, later on."
                ]
            ]
        ]
