module Covariance exposing (main)

import Browser
import Html exposing (Html, button, div, input)
import Html.Attributes as HA
import Html.Events as HE
import List exposing (sum)
import Maybe
import Random
import Svg
import Svg.Attributes as SA
import Svg.Events as SE


type alias Model =
    { numPoints : Int
    , points : List ( Float, Float )
    , hoveredIndex : Maybe Int
    }


init : Model
init =
    { numPoints = 10
    , points = []
    , hoveredIndex = Nothing
    }


type Msg
    = ChangeNumPoints String
    | Generate
    | GotRandomPoints (List ( Float, Float ))
    | Hover Int
    | Unhover



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeNumPoints str ->
            let
                maybeInt =
                    String.toInt str
                        |> Maybe.andThen
                            (\n ->
                                if n < 2 then
                                    Nothing

                                else if n > 100 then
                                    Nothing

                                else
                                    Just n
                            )

                -- We only allow 2..100
            in
            case maybeInt of
                Just validN ->
                    ( { model | numPoints = validN }, Cmd.none )

                Nothing ->
                    -- If it's not a valid integer, or out of range, just ignore
                    ( model, Cmd.none )

        Generate ->
            -- Generate random points within [0,1] × [0,1]
            let
                gen =
                    Random.list model.numPoints (Random.pair (Random.float 0 1) (Random.float 0 1))
            in
            ( model, Random.generate GotRandomPoints gen )

        GotRandomPoints newPoints ->
            ( { model | points = newPoints, hoveredIndex = Nothing }, Cmd.none )

        Hover idx ->
            ( { model | hoveredIndex = Just idx }, Cmd.none )

        Unhover ->
            ( { model | hoveredIndex = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ HA.style "width" "100vw"
        , HA.style "height" "100vh"
        , HA.style "box-sizing" "border-box"
        , HA.style "margin" "0"
        , HA.style "padding" "0"
        ]
        [ -- Controls
          div [ HA.style "padding" "1rem" ]
            [ Html.text "Number of points (2..100): "
            , input
                [ HA.type_ "number"
                , HA.value (String.fromInt model.numPoints)
                , HA.min "2"
                , HA.max "100"
                , HE.onInput ChangeNumPoints
                ]
                []
            , button [ HE.onClick Generate ] [ Html.text "Generate / Regenerate Random Points" ]
            ]
        , -- SVG Graph
          viewGraph model
        ]


viewGraph : Model -> Html Msg
viewGraph model =
    let
        -- Dimensions
        width =
            800

        height =
            600

        margin =
            40

        -- Convert data coordinate (0..1) to SVG coordinate
        -- We consider y increasing upwards. However, in SVG, y grows downwards.
        -- We'll invert the y-axis by subtracting from total height.
        scaleX x =
            margin + x * toFloat (width - 2 * margin)

        scaleY y =
            (toFloat height - margin) - y * toFloat (height - 2 * margin)

        -- Averages
        meanX =
            if List.length model.points == 0 then
                0

            else
                List.map (\( x, _ ) -> x) model.points
                    |> sum
                    |> (\s -> s / toFloat (List.length model.points))

        meanY =
            if List.length model.points == 0 then
                0

            else
                List.map (\( _, y ) -> y) model.points
                    |> sum
                    |> (\s -> s / toFloat (List.length model.points))

        -- Transform means
        meanXSvg =
            scaleX meanX

        meanYSvg =
            scaleY meanY
    in
    Svg.svg
        [ SA.width (String.fromInt width)
        , SA.height (String.fromInt height)
        , SA.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
        , SA.style "background-color:white"
        ]
        ([ -- X axis
           Svg.line
            [ SA.x1 (String.fromFloat <| toFloat margin)
            , SA.y1 (String.fromFloat <| toFloat (height - margin))
            , SA.x2 (String.fromFloat <| toFloat (width - margin))
            , SA.y2 (String.fromFloat <| toFloat (height - margin))
            , SA.stroke "black"
            , SA.strokeWidth "2"
            ]
            []
         , -- Y axis
           Svg.line
            [ SA.x1 (String.fromFloat <| toFloat margin)
            , SA.y1 (String.fromFloat <| toFloat margin)
            , SA.x2 (String.fromFloat <| toFloat margin)
            , SA.y2 (String.fromFloat <| toFloat (height - margin))
            , SA.stroke "black"
            , SA.strokeWidth "2"
            ]
            []
         , -- Mean X line
           Svg.line
            [ SA.x1 (String.fromFloat meanXSvg)
            , SA.y1 (String.fromFloat <| toFloat margin)
            , SA.x2 (String.fromFloat meanXSvg)
            , SA.y2 (String.fromFloat <| toFloat (height - margin))
            , SA.stroke "grey"
            , SA.strokeDasharray "4,4"
            ]
            []
         , -- Mean Y line
           Svg.line
            [ SA.x1 (String.fromFloat <| toFloat margin)
            , SA.y1 (String.fromFloat meanYSvg)
            , SA.x2 (String.fromFloat <| toFloat (width - margin))
            , SA.y2 (String.fromFloat meanYSvg)
            , SA.stroke "grey"
            , SA.strokeDasharray "4,4"
            ]
            []
         ]
            ++ -- Hovered "square" (rectangle) from (meanX, meanY) to the hovered point
               (case model.hoveredIndex of
                    Just i ->
                        case List.drop i model.points |> List.head of
                            Just ( px, py ) ->
                                let
                                    pxSvg =
                                        scaleX px

                                    pySvg =
                                        scaleY py

                                    sign =
                                        (px - meanX) * (py - meanY)

                                    fillColor =
                                        if sign >= 0 then
                                            "blue"

                                        else
                                            "red"

                                    rectX =
                                        Basics.min pxSvg meanXSvg

                                    rectY =
                                        Basics.min pySvg meanYSvg

                                    rectWidth =
                                        abs (pxSvg - meanXSvg)

                                    rectHeight =
                                        abs (pySvg - meanYSvg)
                                in
                                [ Svg.rect
                                    [ SA.x (String.fromFloat rectX)
                                    , SA.y (String.fromFloat rectY)
                                    , SA.width (String.fromFloat rectWidth)
                                    , SA.height (String.fromFloat rectHeight)
                                    , SA.fill fillColor
                                    , SA.fillOpacity "0.3"
                                    ]
                                    []
                                ]

                            Nothing ->
                                []

                    Nothing ->
                        []
               )
            ++ -- Points
               List.indexedMap
                (\i ( x, y ) ->
                    let
                        cx_ =
                            scaleX x

                        cy_ =
                            scaleY y
                    in
                    Svg.circle
                        [ SA.cx (String.fromFloat cx_)
                        , SA.cy (String.fromFloat cy_)
                        , SA.r "5"
                        , SA.fill "black"
                        , SE.onMouseOver (Hover i)
                        , SE.onMouseOut Unhover
                        ]
                        []
                )
                model.points
        )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
