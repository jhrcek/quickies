module Covariance exposing (main)

import Browser
import Browser.Events as Events
import Html exposing (Html, button, div, input, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import List
import Maybe exposing (Maybe(..))
import Random
import Svg
import Svg.Attributes as SA
import Svg.Events as SE


type alias Model =
    { numPoints : Int
    , points : List ( Float, Float )
    , hoveredIndex : Maybe Int
    , dragState : Maybe DragState
    }


type alias DragState =
    { index : Int
    , offsetX : Float
    , offsetY : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { numPoints = 10
      , points = []
      , hoveredIndex = Nothing
      , dragState = Nothing
      }
    , generatePoints 10
    )


type Msg
    = ChangeNumPoints String
    | Generate
    | GotRandomPoints (List ( Float, Float ))
    | Hover Int
    | Unhover
    | SummationHover Int
    | SummationUnhover
    | StartDrag Int Float Float
    | Move Float Float
    | StopDrag


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeNumPoints str ->
            let
                maybeInt =
                    String.toInt str
                        |> Maybe.andThen
                            (\n ->
                                if 1 <= n && n <= 100 then
                                    Just n

                                else
                                    Nothing
                            )
            in
            ( case maybeInt of
                Just validN ->
                    { model | numPoints = validN }

                Nothing ->
                    model
            , Cmd.none
            )

        Generate ->
            ( model, generatePoints model.numPoints )

        GotRandomPoints newPoints ->
            ( { model | points = newPoints, hoveredIndex = Nothing }, Cmd.none )

        Hover idx ->
            ( { model | hoveredIndex = Just idx }, Cmd.none )

        Unhover ->
            case model.dragState of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    ( { model | hoveredIndex = Nothing }, Cmd.none )

        SummationHover idx ->
            ( { model | hoveredIndex = Just idx }, Cmd.none )

        SummationUnhover ->
            case model.dragState of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    ( { model | hoveredIndex = Nothing }, Cmd.none )

        StartDrag index mouseX mouseY ->
            let
                ( screenX, screenY ) =
                    pointToScreen model index

                offsetX =
                    screenX - mouseX

                offsetY =
                    screenY - mouseY
            in
            ( { model | dragState = Just { index = index, offsetX = offsetX, offsetY = offsetY } }
            , Cmd.none
            )

        Move mouseX mouseY ->
            case model.dragState of
                Just d ->
                    let
                        newScreenX =
                            mouseX + d.offsetX

                        newScreenY =
                            mouseY + d.offsetY

                        ( newDataX, newDataY ) =
                            screenToData newScreenX newScreenY

                        clampedX =
                            clamp 0 1 newDataX

                        clampedY =
                            clamp 0 1 newDataY

                        updatedPoints =
                            List.indexedMap
                                (\i ( oldX, oldY ) ->
                                    if i == d.index then
                                        ( clampedX, clampedY )

                                    else
                                        ( oldX, oldY )
                                )
                                model.points
                    in
                    ( { model | points = updatedPoints }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        StopDrag ->
            ( { model | dragState = Nothing }, Cmd.none )


generatePoints : Int -> Cmd Msg
generatePoints numPoints =
    Random.generate GotRandomPoints (Random.list numPoints (Random.pair (Random.float 0 1) (Random.float 0 1)))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragState of
        Just _ ->
            Sub.batch
                [ Events.onMouseMove (D.map2 Move (D.field "clientX" D.float) (D.field "clientY" D.float))
                , Events.onMouseUp (D.succeed StopDrag)
                ]

        Nothing ->
            Sub.none


view : Model -> Html Msg
view model =
    div
        [ HA.style "width" "100vw"
        , HA.style "height" "100vh"
        , HA.style "box-sizing" "border-box"
        , HA.style "margin" "0"
        , HA.style "padding" "0"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        ]
        [ Html.h1 [] [ Html.text "Covariance" ]
        , controlsView model
        , div
            [ HA.style "display" "flex"
            , HA.style "flex" "1"
            , HA.style "flex-direction" "row"
            , HA.style "padding" "1rem"
            ]
            [ div
                [ HA.style "flex" "0 0 auto" -- chart gets natural width from the SVG
                ]
                [ viewGraph model ]
            , div
                [ HA.style "flex" "1"
                , HA.style "margin-left" "1rem"
                , HA.style "overflow" "auto"
                ]
                [ viewExpandedFormula model ]
            ]
        ]


controlsView : Model -> Html Msg
controlsView model =
    div [ HA.style "padding" "1rem" ]
        [ text "Number of points "
        , input
            [ HA.type_ "number"
            , HA.value (String.fromInt model.numPoints)
            , HA.min "1"
            , HA.max "100"
            , HE.onInput ChangeNumPoints
            ]
            []
        , button [ HE.onClick Generate ] [ text "Generate Random Points" ]
        ]


viewGraph : Model -> Html Msg
viewGraph model =
    let
        ( meanX, meanY, _ ) =
            meanXY model.points

        meanXSvg =
            scaleX meanX

        meanYSvg =
            scaleY meanY

        highlightedIndex =
            case model.dragState of
                Just d ->
                    Just d.index

                Nothing ->
                    model.hoveredIndex

        highlightRect =
            case highlightedIndex of
                Just i ->
                    case List.head (List.drop i model.points) of
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
                                    min pxSvg meanXSvg

                                rectY =
                                    min pySvg meanYSvg

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
        [ SA.width (String.fromFloat svgWidth)
        , SA.height (String.fromFloat svgHeight)
        , SA.viewBox ("0 0 " ++ String.fromFloat svgWidth ++ " " ++ String.fromFloat svgHeight)
        , SA.style "background-color:white"
        ]
        ([ Svg.line
            [ SA.x1 (String.fromFloat svgMargin)
            , SA.y1 (String.fromFloat (svgHeight - svgMargin))
            , SA.x2 (String.fromFloat (svgWidth - svgMargin))
            , SA.y2 (String.fromFloat (svgHeight - svgMargin))
            , SA.stroke "black"
            , SA.strokeWidth "2"
            ]
            []
         , Svg.line
            [ SA.x1 (String.fromFloat svgMargin)
            , SA.y1 (String.fromFloat svgMargin)
            , SA.x2 (String.fromFloat svgMargin)
            , SA.y2 (String.fromFloat (svgHeight - svgMargin))
            , SA.stroke "black"
            , SA.strokeWidth "2"
            ]
            []
         , Svg.line
            [ SA.x1 (String.fromFloat meanXSvg)
            , SA.y1 (String.fromFloat svgMargin)
            , SA.x2 (String.fromFloat meanXSvg)
            , SA.y2 (String.fromFloat (svgHeight - svgMargin))
            , SA.stroke "grey"
            , SA.strokeDasharray "4,4"
            ]
            []
         , Svg.line
            [ SA.x1 (String.fromFloat svgMargin)
            , SA.y1 (String.fromFloat meanYSvg)
            , SA.x2 (String.fromFloat (svgWidth - svgMargin))
            , SA.y2 (String.fromFloat meanYSvg)
            , SA.stroke "grey"
            , SA.strokeDasharray "4,4"
            ]
            []
         ]
            ++ highlightRect
            ++ List.indexedMap viewPoint model.points
        )


viewPoint :
    Int
    -> ( Float, Float )
    -> Svg.Svg Msg
viewPoint idx ( x, y ) =
    let
        cx_ =
            scaleX x

        cy_ =
            scaleY y

        titleText =
            "(" ++ round3DP x ++ ", " ++ round3DP y ++ ")"
    in
    Svg.circle
        [ SA.cx (String.fromFloat cx_)
        , SA.cy (String.fromFloat cy_)
        , SA.r "5"
        , SA.fill "black"
        , onMouseDownPos (StartDrag idx)
        , SE.onMouseOver (Hover idx)
        , SE.onMouseOut Unhover
        ]
        [ Svg.title []
            [ Svg.text titleText ]
        ]


meanXY : List ( Float, Float ) -> ( Float, Float, Float )
meanXY points =
    if List.isEmpty points then
        ( 0, 0, 0 )

    else
        let
            n =
                toFloat (List.length points)

            ( sumX, sumY ) =
                List.foldl (\( x, y ) ( accX, accY ) -> ( x + accX, y + accY )) ( 0, 0 ) points
        in
        ( sumX / n, sumY / n, n )


viewExpandedFormula : Model -> Html Msg
viewExpandedFormula model =
    let
        ( meanX, meanY, n ) =
            meanXY model.points

        totalCov =
            if n == 0 then
                0

            else
                List.foldl
                    (\( x, y ) acc -> acc + (x - meanX) * (y - meanY))
                    0
                    model.points
                    / n
    in
    div [ HA.style "margin" "1rem" ]
        [ div []
            [ text ("Cov(X,Y) = 1/n * Σ (xᵢ - x̄)(yᵢ - ȳ) = " ++ round3DP totalCov) ]
        , div []
            (List.indexedMap (viewTerm meanX meanY model.hoveredIndex) model.points)
        ]


viewTerm : Float -> Float -> Maybe Int -> Int -> ( Float, Float ) -> Html Msg
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

        -- If this term is hovered, pick a light background color
        bg =
            case hoveredIndex of
                Just hI ->
                    if hI == i then
                        if product >= 0 then
                            "lightblue"

                        else
                            "pink"

                    else
                        "transparent"

                Nothing ->
                    "transparent"

        content =
            "("
                ++ round3DP dx
                ++ ")·("
                ++ round3DP dy
                ++ ") = "
                ++ round3DP product
    in
    div
        [ HA.style "margin" "6px 0"
        , HE.onMouseOver (SummationHover i)
        , HE.onMouseOut SummationUnhover
        ]
        [ Html.span
            [ HA.style "display" "inline-block"
            , HA.style "color" color
            , HA.style "background-color" bg
            , HA.style "padding" "4px"
            ]
            [ text content ]
        ]


onMouseDownPos : (Float -> Float -> msg) -> Svg.Attribute msg
onMouseDownPos toMsg =
    let
        decoder =
            D.map2 toMsg
                (D.field "clientX" D.float)
                (D.field "clientY" D.float)
    in
    HE.on "mousedown" decoder


pointToScreen : Model -> Int -> ( Float, Float )
pointToScreen model i =
    let
        ( x, y ) =
            List.head (List.drop i model.points)
                |> Maybe.withDefault ( 0, 0 )
    in
    ( scaleX x, scaleY y )


screenToData : Float -> Float -> ( Float, Float )
screenToData screenX screenY =
    let
        dataX =
            (screenX - svgMargin) / (svgWidth - 2 * svgMargin)

        dataY =
            (svgHeight - svgMargin - screenY) / (svgHeight - 2 * svgMargin)
    in
    ( dataX, dataY )


scaleX : Float -> Float
scaleX x =
    svgMargin + x * (svgWidth - 2 * svgMargin)


scaleY : Float -> Float
scaleY y =
    svgHeight - svgMargin - y * (svgHeight - 2 * svgMargin)


svgWidth : Float
svgWidth =
    800


svgHeight : Float
svgHeight =
    600


svgMargin : Float
svgMargin =
    40


round3DP : Float -> String
round3DP val =
    let
        rounded =
            round (val * 1000)
    in
    String.fromFloat (toFloat rounded / 1000)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
