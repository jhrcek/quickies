module Lesson.BruteForce exposing (Model, Msg, init, subscriptions, update, view)

import Dict
import Html exposing (Html, button, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, title)
import Html.Events exposing (onClick)
import Random
import Sat.Formula as F exposing (Formula)
import Sat.Generate as G
import Time
import Ui exposing (em, insight, p, strong, t)


type alias Model =
    { formula : Formula
    , checked : Int
    , playing : Bool
    , selected : Maybe Int
    , seed : Random.Seed
    }


type Msg
    = Step
    | Play
    | Pause
    | Tick
    | Reset
    | Select Int
    | New


n : Int
n =
    5


init : Random.Seed -> Model
init seed0 =
    let
        ( f, seed ) =
            Random.step (G.formula n 16 3) seed0
    in
    { formula = f, checked = 0, playing = False, selected = Nothing, seed = seed }


total : Int
total =
    2 ^ n


update : Msg -> Model -> Model
update msg m =
    case msg of
        Step ->
            { m | checked = min total (m.checked + 1), selected = Just m.checked }

        Tick ->
            if m.checked >= total then
                { m | playing = False }

            else
                { m | checked = m.checked + 1, selected = Just m.checked }

        Play ->
            { m | playing = True }

        Pause ->
            { m | playing = False }

        Reset ->
            { m | checked = 0, playing = False, selected = Nothing }

        Select i ->
            { m | selected = Just i }

        New ->
            init m.seed


subscriptions : Model -> Sub Msg
subscriptions m =
    if m.playing then
        Time.every 90 (\_ -> Tick)

    else
        Sub.none


view : Model -> Html Msg
view m =
    let
        solutions =
            List.range 0 (m.checked - 1) |> List.filter (\i -> F.satisfies m.formula (F.assignmentFromIndex n i)) |> List.length
    in
    div []
        [ p [ t "The obvious algorithm: try every assignment, one after another, and check each. It always works. Here are all 32 assignments of a random 5-variable formula; each square is one assignment, counted in binary." ]
        , Ui.row
            [ Ui.btnIf (m.checked < total) "Check next" Step
            , if m.playing then
                Ui.btnPrimary "Pause" Pause

              else
                Ui.btnPrimary "Check all" Play
            , Ui.btn "Reset" Reset
            , Ui.btn "New formula" New
            ]
        , div [ class "grid32" ]
            (List.range 0 (total - 1)
                |> List.map
                    (\i ->
                        let
                            state =
                                if i >= m.checked then
                                    "cell"

                                else
                                    let
                                        a =
                                            F.assignmentFromIndex n i
                                    in
                                    if F.satisfies m.formula a then
                                        "cell yes"

                                    else
                                        "cell no"
                        in
                        button
                            [ class state
                            , classList [ ( "sel", m.selected == Just i ) ]
                            , onClick (Select i)
                            , title (bits i)
                            ]
                            [ text (bits i) ]
                    )
            )
        , Ui.stats
            [ Ui.stat "checked" (String.fromInt m.checked ++ " / " ++ String.fromInt total)
            , Ui.stat "solutions found" (String.fromInt solutions)
            ]
        , case m.selected of
            Just i ->
                selectedView m.formula i

            Nothing ->
                text ""
        , Ui.h3 "The wall"
        , p [ t "Each new variable doubles the work. Suppose we check a billion assignments per second:" ]
        , growthTable
        , insight "Why nobody has beaten the wall"
            [ p
                [ t "In 1971 Cook (and independently Levin) showed SAT is "
                , strong "NP-complete"
                , t ": every problem in NP can be translated into SAT efficiently. A polynomial-time algorithm for 3-SAT would therefore prove P = NP, the most famous open problem in computer science."
                ]
            , p
                [ t "The best proven worst-case bounds for 3-SAT are around O(1.307ⁿ), from refinements of the randomized PPSZ algorithm (Paturi, Pudlák, Saks, Zane) — a huge improvement over 2ⁿ, still exponential. The "
                , em "Exponential Time Hypothesis"
                , t " (Impagliazzo and Paturi, 1999) conjectures this is unavoidable: there is a δ > 0 such that 3-SAT has no O(2^(δn)) algorithm. The stronger SETH says the base for k-SAT must approach 2 as k grows."
                ]
            , p [ t "So the rest of this journey is not about beating the wall in the worst case. It is about never walking into it on the formulas people actually care about." ]
            ]
        ]


bits : Int -> String
bits i =
    F.assignmentFromIndex n i
        |> Dict.values
        |> List.map
            (\b ->
                if b then
                    "1"

                else
                    "0"
            )
        |> String.concat


selectedView : Formula -> Int -> Html msg
selectedView f i =
    let
        a =
            F.assignmentFromIndex n i

        broken =
            List.filter (not << F.isSatisfiedBy a) f.clauses
    in
    div [ class "panel" ]
        [ p
            [ strong (bits i)
            , t " means "
            , t
                (Dict.toList a
                    |> List.map
                        (\( v, b ) ->
                            Ui.varName v
                                ++ "="
                                ++ (if b then
                                        "T"

                                    else
                                        "F"
                                   )
                        )
                    |> String.join ", "
                )
            , t "."
            ]
        , case broken of
            [] ->
                p [ t "Every clause is satisfied: a solution." ]

            _ ->
                div []
                    [ p [ t ("It violates " ++ String.fromInt (List.length broken) ++ " clause(s):") ]
                    , Ui.viewClauses a (always Ui.NoMark) broken
                    ]
        ]


growthTable : Html msg
growthTable =
    table [ class "tt num" ]
        [ thead [] [ tr [] [ th [] [ text "variables" ], th [] [ text "assignments" ], th [] [ text "time" ] ] ]
        , tbody []
            (List.map
                (\k ->
                    tr []
                        [ td [] [ text (String.fromInt k) ]
                        , td [] [ text (sci (2 ^ toFloat k)) ]
                        , td [] [ text (duration (2 ^ toFloat k / 1.0e9)) ]
                        ]
                )
                [ 10, 20, 30, 40, 50, 60, 80, 100, 300 ]
            )
        ]


sci : Float -> String
sci x =
    if x < 1.0e6 then
        String.fromInt (round x)

    else
        let
            e =
                floor (logBase 10 x)

            mant =
                x / (10 ^ toFloat e)
        in
        String.fromFloat (toFloat (round (mant * 10)) / 10) ++ " × 10" ++ sup e


sup : Int -> String
sup e =
    String.fromInt e
        |> String.toList
        |> List.map
            (\c ->
                case c of
                    '0' ->
                        '⁰'

                    '1' ->
                        '¹'

                    '2' ->
                        '²'

                    '3' ->
                        '³'

                    '4' ->
                        '⁴'

                    '5' ->
                        '⁵'

                    '6' ->
                        '⁶'

                    '7' ->
                        '⁷'

                    '8' ->
                        '⁸'

                    '9' ->
                        '⁹'

                    _ ->
                        c
            )
        |> String.fromList


duration : Float -> String
duration s =
    if s < 1.0e-3 then
        "instant"

    else if s < 1 then
        String.fromInt (round (s * 1000)) ++ " ms"

    else if s < 120 then
        String.fromInt (round s) ++ " s"

    else if s < 7200 then
        String.fromInt (round (s / 60)) ++ " min"

    else if s < 2 * 86400 then
        String.fromInt (round (s / 3600)) ++ " h"

    else
        let
            year =
                3.156e7
        in
        if s < year then
            String.fromInt (round (s / 86400)) ++ " days"

        else if s < 1000 * year then
            String.fromInt (round (s / year)) ++ " years"

        else
            let
                universe =
                    1.38e10 * year
            in
            if s < universe then
                sci (s / year) ++ " years"

            else
                sci (s / universe) ++ " × the age of the universe"
