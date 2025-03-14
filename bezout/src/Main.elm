module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { a : Int
    , b : Int
    , steps : List EuclidStep
    }


type alias EuclidStep =
    { a : Int
    , b : Int
    , q : Int
    , r : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { a = 86
      , b = 13
      , steps = euclidSteps 86 13
      }
    , Cmd.none
    )


type Msg
    = UpdateA String
    | UpdateB String
    | SelectPair Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateA newA ->
            let
                newIntA =
                    String.toInt newA
                        |> Maybe.withDefault model.a
                        |> clamp 1 100
            in
            ( { model | a = newIntA, steps = euclidSteps newIntA model.b }, Cmd.none )

        UpdateB newB ->
            let
                newIntB =
                    String.toInt newB
                        |> Maybe.withDefault model.b
                        |> clamp 1 100
            in
            ( { model | b = newIntB, steps = euclidSteps model.a newIntB }, Cmd.none )

        SelectPair i j ->
            ( { model | a = i, b = j, steps = euclidSteps i j }, Cmd.none )


euclidSteps : Int -> Int -> List EuclidStep
euclidSteps a b =
    if a < b then
        euclidSteps b a

    else if b == 0 then
        []

    else
        let
            q =
                a // b

            r =
                modBy b a

            step =
                { a = a, b = b, q = q, r = r }
        in
        step :: euclidSteps b r


view : Model -> Html Msg
view model =
    div [ style "margin" "20px", style "font-family" "sans-serif" ]
        [ h1 [] [ text "Euclid's Algorithm Visualization" ]
        , div [ style "display" "flex", style "flex-wrap" "wrap", style "gap" "30px", style "align-items" "flex-start" ]
            [ div [ style "flex" "1", style "min-width" "300px" ]
                [ div [ style "margin-bottom" "20px" ]
                    [ p []
                        [ text "Enter numbers: a = "
                        , input
                            [ type_ "number"
                            , value (String.fromInt model.a)
                            , onInput UpdateA
                            , Html.Attributes.min "1"
                            , Html.Attributes.max "100"
                            , style "width" "60px"
                            , style "margin" "0 10px 0 5px"
                            ]
                            []
                        , text ", b = "
                        , input
                            [ type_ "number"
                            , value (String.fromInt model.b)
                            , onInput UpdateB
                            , Html.Attributes.min "1"
                            , Html.Attributes.max "100"
                            , style "width" "60px"
                            , style "margin-left" "5px"
                            ]
                            []
                        ]
                    , p [ style "font-size" "14px", style "margin-top" "8px", style "color" "#555" ]
                        [ text "Or click a cell in the heatmap below. The color indicates the number of steps (white = 1 step, darker = more steps)" ]
                    ]
                , viewHeatmapTable model
                ]
            , div [ style "flex" "1", style "min-width" "300px" ]
                [ h2 [] [ text "Steps of Euclid's Algorithm:" ]
                , viewStepsContent model.steps
                ]
            ]
        ]


viewStepsContent : List EuclidStep -> Html Msg
viewStepsContent steps =
    if List.isEmpty steps then
        div [ style "margin-top" "20px" ]
            [ text "Enter two valid numbers to see the steps of Euclid's algorithm." ]

    else
        ol [ style "line-height" "1.5" ] (List.indexedMap viewStep steps)


viewSteps : List EuclidStep -> Html Msg
viewSteps steps =
    div [ style "margin-top" "20px" ]
        [ h2 [] [ text "Steps of Euclid's Algorithm:" ]
        , viewStepsContent steps
        ]


viewStep : Int -> EuclidStep -> Html Msg
viewStep index step =
    let
        colorA =
            getColor index

        colorB =
            getColor (index + 1)

        colorR =
            getColor (index + 2)

        styledNumber color num =
            span [ style "color" color, style "font-weight" "bold" ] [ text (String.fromInt num) ]

        equationParts =
            [ styledNumber colorA step.a
            , text " = "
            , styledNumber colorB step.b
            , text " × "
            , text (String.fromInt step.q)
            , text " + "
            , styledNumber colorR step.r
            ]
    in
    li [] equationParts


colors : List String
colors =
    [ "#e41a1c"
    , "#377eb8"
    , "#4daf4a"
    , "#984ea3"
    , "#ff7f00"
    , "#ffff33"
    , "#a65628"
    , "#f781bf"
    , "#999999"
    , "#66c2a5"
    , "#fc8d62"
    , "#8da0cb"
    , "#e78ac3"
    , "#a6d854"
    , "#ffd92f"
    ]


getColor : Int -> String
getColor index =
    case List.drop (modBy (List.length colors) index) colors of
        color :: _ ->
            color

        [] ->
            "#000000"


viewHeatmapTable : Model -> Html Msg
viewHeatmapTable model =
    let
        maxSteps =
            findMaxSteps 100 100
    in
    div [ style "overflow-x" "auto" ]
        [ table
            [ style "border-collapse" "collapse"
            , style "table-layout" "fixed"
            ]
            (List.range 1 100
                |> List.map
                    (\i ->
                        tr []
                            (List.range 1 100
                                |> List.map
                                    (\j ->
                                        let
                                            stepCount =
                                                List.length (euclidSteps i j)

                                            bgcolor =
                                                getStepColor stepCount maxSteps

                                            highlight =
                                                if model.a == i && model.b == j then
                                                    "2px solid #0000ff"

                                                else
                                                    "none"
                                        in
                                        td
                                            [ style "width" "8px"
                                            , style "height" "8px"
                                            , style "background-color" bgcolor
                                            , style "cursor" "pointer"
                                            , style "border" "1px solid #eee"
                                            , style "outline" highlight
                                            , onClick (SelectPair i j)
                                            , title (String.fromInt i ++ "," ++ String.fromInt j ++ " (" ++ String.fromInt stepCount ++ " steps)")
                                            ]
                                            []
                                    )
                            )
                    )
            )
        ]


findMaxSteps : Int -> Int -> Int
findMaxSteps maxI maxJ =
    List.range 1 maxI
        |> List.concatMap
            (\i ->
                List.range 1 maxJ
                    |> List.map
                        (\j ->
                            List.length (euclidSteps i j)
                        )
            )
        |> List.maximum
        |> Maybe.withDefault 1


getStepColor : Int -> Int -> String
getStepColor steps maxSteps =
    -- Calculate shade: 255 for 1 step (white), darker for more steps
    let
        -- Invert the ratio so white = 1 step, dark = max steps
        shade =
            255
                - round (220 * toFloat (steps - 1) / toFloat (maxSteps - 1))
                |> clamp 35 255
    in
    "rgb(" ++ String.fromInt shade ++ "," ++ String.fromInt shade ++ "," ++ String.fromInt shade ++ ")"
