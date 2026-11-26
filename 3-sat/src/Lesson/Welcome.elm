module Lesson.Welcome exposing (view)

import Dict
import Html exposing (Html, button, div, li, ol, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Journey exposing (Step)
import Ui exposing (em, insight, p, strong, t, wiki)


example : List (List Int)
example =
    [ [ 1, -2, 3 ], [ -1, 2, 4 ], [ 2, -3, -4 ], [ -1, -2, -4 ] ]


view : (Step -> msg) -> Html msg
view go =
    div []
        [ p
            [ t "An instance of "
            , Html.strong [] [ wiki "Boolean satisfiability problem#3-satisfiability" "3-SAT" ]
            , t " is a "
            , wiki "Propositional formula" "propositional formula"
            , t " in "
            , wiki "Conjunctive normal form" "conjunctive normal form"
            , t " where every "
            , wiki "Clause (logic)" "clause"
            , t " is a disjunction of exactly three "
            , wiki "Literal (mathematical logic)" "literals"
            , t ". The question is a "
            , wiki "Decision problem" "decision problem"
            , t ": is there an assignment of truth values to the variables that makes "
            , strong "every"
            , t " clause true at once? Read each clause as a constraint saying "
            , em "“at least one of these three literals holds”"
            , t "."
            ]
        , Ui.viewClauses Dict.empty (always Ui.NoMark) example
        , p
            [ t "That is the whole problem. The statement is tiny, yet it sits at the centre of "
            , wiki "Computational complexity theory" "complexity theory"
            , t ": SAT was the first problem proved "
            , wiki "NP-completeness" "NP-complete"
            , t " ("
            , wiki "Cook–Levin theorem" "Cook–Levin"
            , t "), and 3-SAT is the canonical starting point for "
            , wiki "NP-hardness" "NP-hardness"
            , t " "
            , wiki "Polynomial-time reduction" "reductions"
            , t ". A polynomial-time algorithm for it would give a polynomial-time algorithm for every problem in "
            , wiki "NP (complexity)" "NP"
            , t "."
            ]
        , p
            [ t "And still, modern "
            , wiki "SAT solver" "SAT solvers"
            , t " routinely crack industrial formulas with millions of variables: chip verification, software package resolution, scheduling, cryptanalysis, even open problems in mathematics. This journey explains how — from the definitions up to "
            , wiki "Conflict-driven clause learning" "conflict-driven clause learning"
            , t " — with a playground at every step."
            ]
        , insight "How to read this"
            [ p [ t "Every step has something to poke. Play first, then read the notes; the ideas build on each other, but you can jump anywhere from the bar at the top." ]
            ]
        , ol [ class "journey-map" ]
            (Journey.all
                |> List.drop 1
                |> List.map
                    (\s ->
                        li []
                            [ button [ class "map-item", onClick (go s) ]
                                [ span [ class "map-n" ] [ text (String.fromInt (Journey.index s)) ]
                                , span [] [ text (Journey.info s).title ]
                                ]
                            ]
                    )
            )
        ]
