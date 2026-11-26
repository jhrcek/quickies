module Lesson.Clauses exposing (Model, Msg, init, update, view)

import Dict
import Html exposing (Html, button, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import ListX as LE
import Sat.Formula as F exposing (Assignment, Clause)
import Ui exposing (em, insight, p, strong, t)


type alias Model =
    { clause : Clause
    , assignment : Assignment
    }


type Msg
    = Toggle Int
    | FlipSign Int
    | Pick Int


init : Model
init =
    { clause = [ 1, -2, 3 ], assignment = F.assignmentFromIndex 3 5 }


update : Msg -> Model -> Model
update msg m =
    case msg of
        Toggle v ->
            { m | assignment = Dict.update v (Maybe.map not) m.assignment }

        FlipSign i ->
            { m | clause = LE.updateAt i negate m.clause }

        Pick i ->
            { m | assignment = F.assignmentFromIndex 3 i }


view : Model -> Html Msg
view m =
    let
        vetoed =
            -- the one assignment that makes every literal false
            List.map (\l -> ( abs l, l < 0 )) m.clause |> Dict.fromList
    in
    div []
        [ p
            [ t "A "
            , strong "clause"
            , t " is an OR of literals. It is true as soon as "
            , em "one"
            , t " of its literals is true, and false only when all of them are false. In 3-SAT every clause has three literals."
            ]
        , p [ t "Tap a literal in the clause to flip its sign; tap a row of the table (or the switches) to try an assignment." ]
        , div [ class "big-clause" ]
            (m.clause
                |> List.indexedMap
                    (\i l ->
                        button [ class (litClass m.assignment l), onClick (FlipSign i) ] [ text (Ui.litName l) ]
                    )
                |> List.intersperse (Html.span [ class "or" ] [ text "∨" ])
            )
        , Ui.varToggles 3 m.assignment (always Ui.Free) Toggle
        , table [ class "tt clickable" ]
            [ thead [] [ tr [] (List.map (\v -> th [] [ text (Ui.varName v) ]) [ 1, 2, 3 ] ++ [ th [] [ text "clause" ] ]) ]
            , tbody []
                (List.range 0 7
                    |> List.map
                        (\i ->
                            let
                                a =
                                    F.assignmentFromIndex 3 i

                                sat =
                                    F.isSatisfiedBy a m.clause
                            in
                            tr
                                [ onClick (Pick i)
                                , classList [ ( "row-false", not sat ), ( "row-current", a == m.assignment ) ]
                                ]
                                (List.map (\v -> td [] [ text (tf (Dict.get v a)) ]) [ 1, 2, 3 ]
                                    ++ [ td []
                                            [ text
                                                (if sat then
                                                    "true"

                                                 else
                                                    "false"
                                                )
                                            ]
                                       ]
                                )
                        )
                )
            ]
        , p
            [ t "Exactly one row is red: "
            , strong (List.map (\v -> Ui.varName v ++ "=" ++ tf (Dict.get v vetoed)) [ 1, 2, 3 ] |> String.join ", ")
            , t ". So think of a clause as a "
            , em "veto"
            , t ": it forbids one specific combination of its three variables and allows the other seven. Flip a sign and watch the veto move."
            ]
        , insight "7/8 and a free theorem"
            [ p [ t "A random assignment satisfies a 3-clause with probability 7/8. Add this up over m clauses: on average a random assignment leaves m/8 clauses unsatisfied." ]
            , p
                [ t "If m < 8 that average is below 1, so "
                , em "some"
                , t " assignment leaves zero unsatisfied: every 3-CNF with at most 7 clauses is satisfiable, without searching at all. This is the probabilistic method in one line. The smallest unsatisfiable 3-CNF is the 8 clauses vetoing all 8 sign patterns of the same three variables."
                ]
            , p [ t "The same averaging shows you can always satisfy 7/8 of the clauses. Håstad proved (2001) that guaranteeing even slightly more than 7/8 in polynomial time is NP-hard, so this trivial bound is exactly optimal." ]
            ]
        ]


tf : Maybe Bool -> String
tf b =
    case b of
        Just True ->
            "T"

        Just False ->
            "F"

        Nothing ->
            "?"


litClass : Assignment -> Int -> String
litClass a l =
    case F.literalValue a l of
        Just True ->
            "lit big is-t"

        Just False ->
            "lit big is-f"

        Nothing ->
            "lit big is-u"
