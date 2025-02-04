module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import List
import String
import Svg
import Svg.Attributes as SA


type alias Model =
    { p : Float
    , n : Int
    }


init : Model
init =
    { p = 0.5, n = 10 }


type Msg
    = UpdateProbability String
    | UpdateTrials String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateProbability newP ->
            case String.toFloat newP of
                Just pValue ->
                    { model | p = clamp 0 1 pValue }

                Nothing ->
                    model

        UpdateTrials newN ->
            case String.toInt newN of
                Just nValue ->
                    { model | n = clamp 1 100 nValue }

                Nothing ->
                    model


binomCoefficient : Int -> Int -> Float
binomCoefficient n k =
    List.foldl (\i acc -> acc * toFloat (n - i + 1) / toFloat i) 1 (List.range 1 k)


binomProbability : Int -> Int -> Float -> Float
binomProbability n k p =
    binomCoefficient n k * (p ^ toFloat k) * ((1 - p) ^ toFloat (n - k))


pmf : Model -> List ( Int, Float )
pmf model =
    List.map (\k -> ( k, binomProbability model.n k model.p )) (List.range 0 model.n)


view : Model -> Html Msg
view model =
    Html.div
        [ HA.style "font-family" "sans-serif"
        , HA.style "margin" "20px"
        ]
        [ Html.h1 [] [ Html.text "Binomial Distribution Visualization" ]
        , Html.div [ HA.style "margin-bottom" "20px" ]
            [ Html.label [] [ Html.text "Success Probability (p): " ]
            , Html.input
                [ HA.type_ "range"
                , HA.min "0"
                , HA.max "1"
                , HA.step "0.01"
                , HA.value (String.fromFloat model.p)
                , HE.onInput UpdateProbability
                ]
                []
            , Html.text (" " ++ String.fromFloat model.p)
            ]
        , Html.div [ HA.style "margin-bottom" "20px" ]
            [ Html.label [] [ Html.text "Number of Trials (n): " ]
            , Html.input
                [ HA.type_ "number"
                , HA.min "1"
                , HA.max "100"
                , HA.value (String.fromInt model.n)
                , HE.onInput UpdateTrials
                ]
                []
            ]
        , svgView model
        ]


svgView : Model -> Html Msg
svgView model =
    let
        totalWidth =
            500

        totalHeight =
            300

        marginLeft =
            60

        marginRight =
            20

        marginTop =
            20

        marginBottom =
            40

        effectiveWidth =
            totalWidth - marginLeft - marginRight

        effectiveHeight =
            totalHeight - marginTop - marginBottom

        probabilities =
            pmf model

        maxProb =
            probabilities
                |> List.map Tuple.second
                |> List.maximum
                |> Maybe.withDefault 1

        barScale =
            0.9

        barWidth =
            toFloat effectiveWidth / toFloat (model.n + 1)

        bars =
            List.map
                (\( k, prob ) ->
                    let
                        xPos =
                            toFloat marginLeft + (toFloat k * barWidth)

                        barHeight =
                            if maxProb > 0 then
                                (prob / maxProb) * (toFloat effectiveHeight * barScale)

                            else
                                0

                        yPos =
                            toFloat marginTop + (toFloat effectiveHeight - barHeight)

                        rectWidth =
                            barWidth * 0.8
                    in
                    Svg.rect
                        [ SA.x (String.fromFloat xPos)
                        , SA.y (String.fromFloat yPos)
                        , SA.width (String.fromFloat rectWidth)
                        , SA.height (String.fromFloat barHeight)
                        , SA.fill "steelblue"
                        ]
                        []
                )
                probabilities

        xTickStep =
            if model.n > 20 then
                5

            else
                1

        xTicks =
            List.filter (\k -> (modBy xTickStep k == 0) || (k == model.n)) (List.range 0 model.n)

        xAxisTicks =
            List.map
                (\k ->
                    let
                        xPos =
                            toFloat marginLeft + (toFloat k * barWidth) + (barWidth / 2)
                    in
                    Svg.text_
                        [ SA.x (String.fromFloat xPos)
                        , SA.y (String.fromInt (totalHeight - 5))
                        , SA.fontSize "10px"
                        , SA.textAnchor "middle"
                        ]
                        [ Svg.text (String.fromInt k) ]
                )
                xTicks

        yTickValues =
            [ 0, maxProb * 0.25, maxProb * 0.5, maxProb * 0.75, maxProb ]

        yAxisTicks =
            List.map
                (\tick ->
                    let
                        yPos =
                            toFloat marginTop
                                + (toFloat effectiveHeight
                                    - (if maxProb > 0 then
                                        (tick / maxProb) * (toFloat effectiveHeight * barScale)

                                       else
                                        0
                                      )
                                  )
                    in
                    Svg.g []
                        [ Svg.line
                            [ SA.x1 (String.fromInt (marginLeft - 5))
                            , SA.y1 (String.fromFloat yPos)
                            , SA.x2 (String.fromInt marginLeft)
                            , SA.y2 (String.fromFloat yPos)
                            , SA.stroke "black"
                            , SA.strokeWidth "1"
                            ]
                            []
                        , Svg.text_
                            [ SA.x (String.fromInt (marginLeft - 10))
                            , SA.y (String.fromFloat (yPos + 3))
                            , SA.fontSize "10px"
                            , SA.textAnchor "end"
                            ]
                            [ Svg.text (formatPercent tick) ]
                        ]
                )
                yTickValues

        xAxisLine =
            Svg.line
                [ SA.x1 (String.fromInt marginLeft)
                , SA.y1 (String.fromInt (marginTop + effectiveHeight))
                , SA.x2 (String.fromInt (marginLeft + effectiveWidth))
                , SA.y2 (String.fromInt (marginTop + effectiveHeight))
                , SA.stroke "black"
                , SA.strokeWidth "1"
                ]
                []

        yAxisLine =
            Svg.line
                [ SA.x1 (String.fromInt marginLeft)
                , SA.y1 (String.fromInt marginTop)
                , SA.x2 (String.fromInt marginLeft)
                , SA.y2 (String.fromInt (marginTop + effectiveHeight))
                , SA.stroke "black"
                , SA.strokeWidth "1"
                ]
                []

        xAxisLabel =
            Svg.text_
                [ SA.x (String.fromFloat (toFloat marginLeft + (toFloat effectiveWidth / 2)))
                , SA.y (String.fromInt (totalHeight - 5 + 15))
                , SA.fontSize "12px"
                , SA.textAnchor "middle"
                ]
                [ Svg.text "Number of Successes" ]

        yAxisLabel =
            Svg.text_
                [ SA.x (String.fromInt 10)
                , SA.y (String.fromFloat (toFloat marginTop + effectiveHeight / 2))
                , SA.fontSize "12px"
                , SA.textAnchor "middle"
                , SA.transform ("rotate(-90, 10," ++ String.fromFloat (toFloat marginTop + effectiveHeight / 2) ++ ")")
                ]
                [ Svg.text "Probability" ]
    in
    Svg.svg
        [ SA.width (String.fromInt totalWidth)
        , SA.height (String.fromInt totalHeight)
        , HA.style "border" "1px solid #ccc"
        ]
        (bars
            ++ [ xAxisLine, yAxisLine ]
            ++ xAxisTicks
            ++ yAxisTicks
            ++ [ xAxisLabel, yAxisLabel ]
        )


formatPercent : Float -> String
formatPercent f =
    let
        percent =
            f * 100

        rounded =
            toFloat (round (percent * 100)) / 100
    in
    String.fromFloat rounded ++ "%"


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
