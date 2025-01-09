module Covariance exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes as HA
import Html.Events as HE
import List exposing (sum)
import Random
import Svg
import Svg.Attributes as SA
import Svg.Events as SE


type alias Model =
    { numPoints : Int
    , points : List ( Float, Float )
    , hoveredIndex : Maybe Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { numPoints = 10
      , points = []
      , hoveredIndex = Nothing
      }
    , generatePoints 10
    )


type Msg
    = ChangeNumPoints String
    | Generate
    | GotRandomPoints (List ( Float, Float ))
    | Hover Int
    | Unhover


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
            in
            case maybeInt of
                Just validN ->
                    ( { model | numPoints = validN }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Generate ->
            ( model, generatePoints model.numPoints )

        GotRandomPoints newPoints ->
            ( { model | points = newPoints, hoveredIndex = Nothing }, Cmd.none )

        Hover idx ->
            ( { model | hoveredIndex = Just idx }, Cmd.none )

        Unhover ->
            ( { model | hoveredIndex = Nothing }, Cmd.none )


generatePoints : Int -> Cmd Msg
generatePoints numPoints =
    let
        gen =
            Random.list numPoints (Random.pair (Random.float 0 1) (Random.float 0 1))
    in
    Random.generate GotRandomPoints gen


view : Model -> Html Msg
view model =
    div
        [ HA.style "width" "100vw"
        , HA.style "height" "100vh"
        , HA.style "box-String.fromIntzing" "border-box"
        , HA.style "margin" "0"
        , HA.style "padding" "0"
        ]
        [ controlsView model
        , viewGraph model
        , viewExpandedFormula model
        ]


controlsView : Model -> Html Msg
controlsView model =
    div [ HA.style "padding" "1rem" ]
        [ text "Number of points (2..100): "
        , input
            [ HA.type_ "number"
            , HA.value (String.fromInt model.numPoints)
            , HA.min "2"
            , HA.max "100"
            , HE.onInput ChangeNumPoints
            ]
            []
        , button [ HE.onClick Generate ] [ text "Generate / Regenerate Random Points" ]
        ]


viewGraph : Model -> Html Msg
viewGraph model =
    let
        width =
            800

        height =
            600

        margin =
            40

        scaleX x =
            margin + x * toFloat (width - 2 * margin)

        scaleY y =
            (toFloat height - margin) - y * toFloat (height - 2 * margin)

        meanX =
            if List.isEmpty model.points then
                0

            else
                let
                    totalX =
                        List.map Tuple.first model.points |> sum
                in
                totalX / toFloat (List.length model.points)

        meanY =
            if List.isEmpty model.points then
                0

            else
                let
                    totalY =
                        List.map Tuple.second model.points |> sum
                in
                totalY / toFloat (List.length model.points)

        meanXSvg =
            scaleX meanX

        meanYSvg =
            scaleY meanY

        hoverRect =
            case model.hoveredIndex of
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
    in
    Svg.svg
        [ SA.width (String.fromInt width)
        , SA.height (String.fromInt height)
        , SA.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
        , SA.style "background-color:white"
        ]
        ([ Svg.line
            [ SA.x1 (String.fromInt margin)
            , SA.y1 (String.fromInt (height - margin))
            , SA.x2 (String.fromInt (width - margin))
            , SA.y2 (String.fromInt (height - margin))
            , SA.stroke "black"
            , SA.strokeWidth "2"
            ]
            []
         , Svg.line
            [ SA.x1 (String.fromInt margin)
            , SA.y1 (String.fromInt margin)
            , SA.x2 (String.fromInt margin)
            , SA.y2 (String.fromInt (height - margin))
            , SA.stroke "black"
            , SA.strokeWidth "2"
            ]
            []
         , Svg.line
            [ SA.x1 (String.fromFloat meanXSvg)
            , SA.y1 (String.fromInt margin)
            , SA.x2 (String.fromFloat meanXSvg)
            , SA.y2 (String.fromInt (height - margin))
            , SA.stroke "grey"
            , SA.strokeDasharray "4,4"
            ]
            []
         , Svg.line
            [ SA.x1 (String.fromInt margin)
            , SA.y1 (String.fromFloat meanYSvg)
            , SA.x2 (String.fromInt (width - margin))
            , SA.y2 (String.fromFloat meanYSvg)
            , SA.stroke "grey"
            , SA.strokeDasharray "4,4"
            ]
            []
         ]
            ++ hoverRect
            ++ List.indexedMap
                (\i ( x, y ) ->
                    let
                        cx_ =
                            scaleX x

                        cy_ =
                            scaleY y

                        titleText =
                            "("
                                ++ truncateFloat x
                                ++ ", "
                                ++ truncateFloat y
                                ++ ")"
                    in
                    Svg.circle
                        [ SA.cx (String.fromFloat cx_)
                        , SA.cy (String.fromFloat cy_)
                        , SA.r "5"
                        , SA.fill "black"
                        , SE.onMouseOver (Hover i)
                        , SE.onMouseOut Unhover
                        ]
                        [ Svg.title []
                            [ Svg.text titleText ]
                        ]
                )
                model.points
        )


{-| Show the expanded covariance formula and each summand (xᵢ - meanX)(yᵢ - meanY).
Color them based on sign, and highlight the hovered item with a border.
-}
viewExpandedFormula : Model -> Html Msg
viewExpandedFormula model =
    let
        meanX =
            if List.isEmpty model.points then
                0

            else
                sum (List.map Tuple.first model.points)
                    / toFloat (List.length model.points)

        meanY =
            if List.isEmpty model.points then
                0

            else
                sum (List.map Tuple.second model.points)
                    / toFloat (List.length model.points)
    in
    div [ HA.style "margin" "1rem" ]
        [ div [] [ text "Cov(X,Y) = Σ (xᵢ -  x̄ ) (yᵢ -  ȳ ), for i = 1..n" ]
        , div []
            (List.indexedMap (viewTerm meanX meanY model.hoveredIndex) model.points)
        ]


viewTerm :
    Float
    -> Float
    -> Maybe Int
    -> Int
    -> ( Float, Float )
    -> Html Msg
viewTerm meanX meanY hoveredIndex i ( x, y ) =
    let
        dx =
            x - meanX

        dy =
            y - meanY

        product =
            dx * dy

        color =
            if product >= 0 then
                "blue"

            else
                "red"

        border =
            case hoveredIndex of
                Just hI ->
                    if hI == i then
                        "2px solid black"

                    else
                        "none"

                Nothing ->
                    "none"

        content =
            "("
                ++ truncateFloat dx
                ++ ")·("
                ++ truncateFloat dy
                ++ ") = "
                ++ truncateFloat product
    in
    div
        [ HA.style "margin" "6px 0"
        , HA.style "color" color
        , HA.style "border" border
        , HA.style "padding" "4px"
        ]
        [ text content ]


{-| Helper to turn Float -> "0.12" with 2 decimals (or fewer if small)
-}
truncateFloat : Float -> String
truncateFloat val =
    let
        rounded =
            round (val * 100)
    in
    String.fromFloat (toFloat rounded / 100)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
