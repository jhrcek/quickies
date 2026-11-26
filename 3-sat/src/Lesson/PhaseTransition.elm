module Lesson.PhaseTransition exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import ListX
import Process
import Random
import Sat.Dpll as D
import Sat.Generate as G
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task
import Ui exposing (em, insight, p, strong, t, wiki)


type alias Point =
    { ratio : Float
    , satFrac : Float
    , medianWork : Float
    }


type alias Batch =
    { ratio : Float, done : Int, sat : Int, work : List Int }


type alias Model =
    { n : Int
    , samples : Int
    , points : List Point
    , pending : List Float
    , batch : Maybe Batch
    , running : Bool
    , seed : Random.Seed
    }


type Msg
    = SetN Int
    | SetSamples Int
    | Run
    | Stop
    | Next


ratios : List Float
ratios =
    List.range 2 16 |> List.map (\i -> toFloat i / 2)


init : Random.Seed -> Model
init seed =
    { n = 20, samples = 20, points = [], pending = [], batch = Nothing, running = False, seed = seed }


next : Cmd Msg
next =
    Task.perform (\_ -> Next) (Process.sleep 0)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        SetN k ->
            ( { m | n = k, running = False, points = [], pending = [], batch = Nothing }, Cmd.none )

        SetSamples k ->
            ( { m | samples = k, running = False, points = [], pending = [], batch = Nothing }, Cmd.none )

        Run ->
            ( { m | points = [], pending = ratios, batch = Nothing, running = True }, next )

        Stop ->
            ( { m | running = False }, Cmd.none )

        Next ->
            if not m.running then
                ( m, Cmd.none )

            else
                case m.batch of
                    Nothing ->
                        case m.pending of
                            r :: rest ->
                                ( { m | pending = rest, batch = Just { ratio = r, done = 0, sat = 0, work = [] } }, next )

                            [] ->
                                ( { m | running = False }, Cmd.none )

                    Just b ->
                        if b.done >= m.samples then
                            ( { m
                                | batch = Nothing
                                , points =
                                    m.points
                                        ++ [ { ratio = b.ratio
                                             , satFrac = toFloat b.sat / toFloat b.done
                                             , medianWork = ListX.median b.work
                                             }
                                           ]
                              }
                            , next
                            )

                        else
                            let
                                ( f, seed ) =
                                    Random.step (G.formula m.n (round (b.ratio * toFloat m.n)) 3) m.seed

                                result =
                                    D.solve f

                                isSat =
                                    case result.status of
                                        D.Sat _ ->
                                            1

                                        _ ->
                                            0
                            in
                            ( { m
                                | seed = seed
                                , batch = Just { b | done = b.done + 1, sat = b.sat + isSat, work = result.stats.conflicts :: b.work }
                              }
                            , next
                            )


view : Model -> Html Msg
view m =
    div []
        [ p
            [ t "Which random 3-SAT formulas are hard? Fix n variables and pick m random clauses. The single number that matters is the "
            , strong "clause density"
            , t " α = m / n."
            ]
        , p [ t "Few clauses: loads of solutions, and a solver stumbles onto one immediately. Very many clauses: the formula is riddled with contradictions, and the solver finds one quickly. In between, something dramatic happens: a ", wiki "Phase transition" "phase transition", t ". Run the experiment: for each density, generate random formulas, solve each with DPLL, and record how many were satisfiable and how hard they were." ]
        , chooser "variables" [ 10, 20, 30 ] m.n SetN
        , chooser "formulas per density" [ 10, 20, 40 ] m.samples SetSamples
        , Ui.row
            [ if m.running then
                Ui.btn "Stop" Stop

              else
                Ui.btnPrimary "Run experiment" Run
            ]
        , div [ class "legend" ]
            [ span [ class "key k-sat" ] [ text "% satisfiable" ]
            , span [ class "key k-work" ] [ text "median DPLL dead ends (conflicts)" ]
            ]
        , div [ class "fig chart" ] [ chart m ]
        , progress m
        , insight "A sharp threshold"
            [ p
                [ t "As n grows, the % satisfiable curve sharpens into a cliff at α ≈ 4.27: below it almost every formula is satisfiable, above it almost none is. And the hardest formulas sit exactly at the cliff — the dead-end curve peaks there, and the peak grows exponentially with n. Try 10, then 30 variables."
                ]
            , p
                [ t "The value 4.267 was predicted by statistical physicists (Mézard, Parisi and Zecchina, 2002) using methods for "
                , wiki "Spin glass" "spin glasses"
                , t ", which also gave a new algorithm, survey propagation. For 3-SAT the exact threshold is still "
                , em "not proven"
                , t "; the best rigorous bounds are roughly 3.52 ≤ α ≤ 4.49. For large k, Ding, Sly and Sun proved the physicists right in 2015."
                ]
            , p [ t "Practical lesson: when people benchmark SAT solvers on random formulas, they pick the threshold density, because elsewhere every solver looks brilliant." ]
            ]
        ]


chooser : String -> List Int -> Int -> (Int -> Msg) -> Html Msg
chooser label options current toMsg =
    div [ class "chooser" ]
        (span [ class "chooser-l" ] [ text label ]
            :: List.map
                (\o ->
                    button [ class "chip", classList [ ( "on", o == current ) ], onClick (toMsg o) ] [ text (String.fromInt o) ]
                )
                options
        )


progress : Model -> Html msg
progress m =
    let
        doneRatios =
            List.length m.points
    in
    if m.running then
        let
            totalRatios =
                List.length ratios
        in
        Ui.eventLine [ text ("Running… density " ++ String.fromInt (doneRatios + 1) ++ " of " ++ String.fromInt totalRatios ++ (m.batch |> Maybe.map (\b -> " (α = " ++ String.fromFloat b.ratio ++ ", formula " ++ String.fromInt b.done ++ "/" ++ String.fromInt m.samples ++ ")") |> Maybe.withDefault "")) ]

    else if doneRatios == 0 then
        Ui.eventLine [ text "Press Run. Each run uses fresh random formulas." ]

    else
        Ui.eventLine [ text ("Done: " ++ String.fromInt (doneRatios * m.samples) ++ " formulas with " ++ String.fromInt m.n ++ " variables solved.") ]



-- CHART


w : Float
w =
    360


h : Float
h =
    230


left : Float
left =
    36


right : Float
right =
    44


top : Float
top =
    14


bottom : Float
bottom =
    32


xOf : Float -> Float
xOf r =
    left + (r - 1) / 7 * (w - left - right)


yOf : Float -> Float
yOf frac =
    top + (1 - frac) * (h - top - bottom)


s : Float -> String
s =
    String.fromFloat


chart : Model -> Svg msg
chart m =
    let
        maxWork =
            m.points |> List.map .medianWork |> List.maximum |> Maybe.withDefault 1 |> max 1

        satPts =
            m.points |> List.map (\pt -> s (xOf pt.ratio) ++ "," ++ s (yOf pt.satFrac)) |> String.join " "

        workPts =
            m.points |> List.map (\pt -> s (xOf pt.ratio) ++ "," ++ s (yOf (pt.medianWork / maxWork))) |> String.join " "

        xTicks =
            List.range 1 8
                |> List.concatMap
                    (\i ->
                        let
                            x =
                                xOf (toFloat i)
                        in
                        [ Svg.line [ SA.x1 (s x), SA.x2 (s x), SA.y1 (s (h - bottom)), SA.y2 (s (h - bottom + 4)), SA.class "axis" ] []
                        , Svg.text_ [ SA.x (s x), SA.y (s (h - bottom + 16)), SA.class "tick mid" ] [ Svg.text (String.fromInt i) ]
                        ]
                    )

        yTicks =
            [ 0, 0.5, 1 ]
                |> List.concatMap
                    (\fr ->
                        [ Svg.line [ SA.x1 (s left), SA.x2 (s (w - right)), SA.y1 (s (yOf fr)), SA.y2 (s (yOf fr)), SA.class "grid" ] []
                        , Svg.text_ [ SA.x (s (left - 5)), SA.y (s (yOf fr + 4)), SA.class "tick end k-sat-t" ] [ Svg.text (String.fromInt (round (fr * 100)) ++ "%") ]
                        , Svg.text_ [ SA.x (s (w - right + 5)), SA.y (s (yOf fr + 4)), SA.class "tick k-work-t" ] [ Svg.text (String.fromInt (round (fr * maxWork))) ]
                        ]
                    )

        threshold =
            [ Svg.line [ SA.x1 (s (xOf 4.27)), SA.x2 (s (xOf 4.27)), SA.y1 (s top), SA.y2 (s (h - bottom)), SA.class "thresh" ] []
            , Svg.text_ [ SA.x (s (xOf 4.27 - 4)), SA.y (s (h - bottom - 6)), SA.class "tick end thresh-l" ] [ Svg.text "4.27" ]
            ]

        dots cls f =
            List.map (\pt -> Svg.circle [ SA.cx (s (xOf pt.ratio)), SA.cy (s (f pt)), SA.r "2.6", SA.class cls ] []) m.points
    in
    Svg.svg [ SA.viewBox ("0 0 " ++ s w ++ " " ++ s h), SA.class "chart-svg" ]
        (yTicks
            ++ xTicks
            ++ threshold
            ++ [ Svg.line [ SA.x1 (s left), SA.x2 (s (w - right)), SA.y1 (s (h - bottom)), SA.y2 (s (h - bottom)), SA.class "axis" ] []
               , Svg.text_ [ SA.x (s ((left + w - right) / 2)), SA.y (s (h - 3)), SA.class "tick mid" ] [ Svg.text "clauses per variable, α = m/n" ]
               , Svg.polyline [ SA.points workPts, SA.class "line-work" ] []
               , Svg.polyline [ SA.points satPts, SA.class "line-sat" ] []
               ]
            ++ dots "dot-work" (\pt -> yOf (pt.medianWork / maxWork))
            ++ dots "dot-sat" (\pt -> yOf pt.satFrac)
        )
