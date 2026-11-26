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
            [ t "A 3-SAT problem is a list of tiny rules about true/false switches. Each rule says "
            , em "“at least one of these three things must hold”"
            , t ". The question: can you set the switches so that "
            , strong "every"
            , t " rule holds at once?"
            ]
        , Ui.viewClauses Dict.empty (always Ui.NoMark) example
        , p [ t "That is the whole problem. It looks like a puzzle for a rainy afternoon, yet it sits at the centre of computer science: 3-SAT was among the first problems shown NP-complete, so a fast algorithm for it would give a fast algorithm for every problem whose answers are easy to check." ]
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
