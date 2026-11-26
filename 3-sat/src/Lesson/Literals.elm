module Lesson.Literals exposing (Model, Msg, init, update, view)

import Dict
import Html exposing (Html, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Sat.Formula exposing (Assignment)
import Ui exposing (code, em, insight, p, strong, t)


type alias Model =
    Assignment


type Msg
    = Toggle Int


init : Model
init =
    Dict.fromList [ ( 1, True ), ( 2, False ), ( 3, False ) ]


update : Msg -> Model -> Model
update (Toggle v) =
    Dict.update v (Maybe.map not)


view : Model -> Html Msg
view a =
    div []
        [ p
            [ t "A "
            , strong "Boolean variable"
            , t " takes a value in {T, F}; we name them x₁, x₂, x₃, … A "
            , strong "literal"
            , t " is a variable (positive literal) or its negation (negative literal). For a literal ℓ we write ℓ̄ for its complement, so the complement of ¬x₂ is x₂. Every variable contributes two complementary literals, and under any assignment exactly one of them is true."
            ]
        , p [ t "Toggle the variables and watch both literals of each." ]
        , Ui.varToggles 3 a (always Ui.Free) Toggle
        , table [ class "tt" ]
            [ thead [] [ tr [] [ th [] [ text "variable" ], th [] [ text "positive literal" ], th [] [ text "negative literal" ] ] ]
            , tbody []
                (List.range 1 3
                    |> List.map
                        (\v ->
                            tr []
                                [ td [] [ text (Ui.varName v) ]
                                , td [] [ Ui.viewLit a v ]
                                , td [] [ Ui.viewLit a (negate v) ]
                                ]
                        )
                )
            ]
        , p
            [ t "An "
            , strong "assignment"
            , t " is a function α : {x₁, …, xₙ} → {T, F}; there are 2ⁿ of them, here 8. A "
            , em "partial"
            , t " assignment is defined only on a subset of the variables (the rest shown as ?). Equivalently, it is a consistent set of literals: one that never contains both ℓ and ℓ̄. Solvers spend almost all their time extending, shrinking and reasoning about partial assignments."
            ]
        , insight "How solvers store literals"
            [ p
                [ t "The standard DIMACS file format writes x₃ as "
                , code "3"
                , t " and ¬x₃ as "
                , code "-3"
                , t ": negation is just "
                , code "negate"
                , t ", and the variable is "
                , code "abs"
                , t ". This app's Elm code does exactly that. MiniSat-style solvers go one step further and store literal 2·v or 2·v+1, so negation is a single XOR with 1 and a literal can index an array directly."
                ]
            ]
        ]
