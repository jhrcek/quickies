module Lesson.Welcome exposing (view)

import Dict
import Html exposing (Html, button, div, li, ol, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Journey exposing (Step)
import Ui exposing (em, insight, p, strong, t)


example : List (List Int)
example =
    [ [ 1, -2, 3 ], [ -1, 2, 4 ], [ 2, -3, -4 ], [ -1, -2, -4 ] ]


view : (Step -> msg) -> Html msg
view go =
    div []
        [ p
            [ t "An instance of "
            , strong "3-SAT"
            , t " is a propositional formula in conjunctive normal form where every clause is a disjunction of exactly three literals. The question is a decision problem: is there an assignment of truth values to the variables that makes "
            , strong "every"
            , t " clause true at once? Read each clause as a constraint saying "
            , em "“at least one of these three literals holds”"
            , t "."
            ]
        , Ui.viewClauses Dict.empty (always Ui.NoMark) example
        , p [ t "That is the whole problem. The statement is tiny, yet it sits at the centre of complexity theory: SAT was the first problem proved NP-complete (Cook–Levin), and 3-SAT is the canonical starting point for NP-hardness reductions. A polynomial-time algorithm for it would give a polynomial-time algorithm for every problem in NP." ]
        , p [ t "And still, modern SAT solvers routinely crack industrial formulas with millions of variables: chip verification, software package resolution, scheduling, cryptanalysis, even open problems in mathematics. This journey explains how — from the definitions up to conflict-driven clause learning — with a playground at every step." ]
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
