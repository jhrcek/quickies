module Covariance exposing (main)

import Browser
import Browser.Events as Events
import Html exposing (Html, button, div, input, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import List exposing (sum)
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
                                if 2 <= n && n <= 100 then
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
                            screenToData model newScreenX newScreenY

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
        [ controlsView model
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
        [ text "Number of points (2..100): "
        , input
            [ HA.type_ "number"
            , HA.value (String.fromInt model.numPoints)
            , HA.min "2"
            , HA.max "100"
            , HE.onInput ChangeNumPoints
            ]
            []
        , button [ HE.onClick Generate ] [ text "Generate Random Points" ]
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

        meanX =
            if List.isEmpty model.points then
                0

            else
                sum (List.map Tuple.first model.points) / toFloat (List.length model.points)

        meanY =
            if List.isEmpty model.points then
                0

            else
                sum (List.map Tuple.second model.points) / toFloat (List.length model.points)

        meanXSvg =
            scaleX margin width meanX

        meanYSvg =
            scaleY margin height meanY

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
                                    scaleX margin width px

                                pySvg =
                                    scaleY margin height py

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
                                [ SA.x (toStr rectX)
                                , SA.y (toStr rectY)
                                , SA.width (toStr rectWidth)
                                , SA.height (toStr rectHeight)
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
            [ SA.x1 (toStr margin)
            , SA.y1 (toStr (height - margin))
            , SA.x2 (toStr (width - margin))
            , SA.y2 (toStr (height - margin))
            , SA.stroke "black"
            , SA.strokeWidth "2"
            ]
            []
         , Svg.line
            [ SA.x1 (toStr margin)
            , SA.y1 (toStr margin)
            , SA.x2 (toStr margin)
            , SA.y2 (toStr (height - margin))
            , SA.stroke "black"
            , SA.strokeWidth "2"
            ]
            []
         , Svg.line
            [ SA.x1 (toStr meanXSvg)
            , SA.y1 (toStr margin)
            , SA.x2 (toStr meanXSvg)
            , SA.y2 (toStr (height - margin))
            , SA.stroke "grey"
            , SA.strokeDasharray "4,4"
            ]
            []
         , Svg.line
            [ SA.x1 (toStr margin)
            , SA.y1 (toStr meanYSvg)
            , SA.x2 (toStr (width - margin))
            , SA.y2 (toStr meanYSvg)
            , SA.stroke "grey"
            , SA.strokeDasharray "4,4"
            ]
            []
         ]
            ++ highlightRect
            ++ List.indexedMap (viewPoint margin width margin height) model.points
        )


viewPoint :
    Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> ( Float, Float )
    -> Svg.Svg Msg
viewPoint margin w margin2 h i ( x, y ) =
    let
        cx_ =
            scaleX margin w x

        cy_ =
            scaleY margin2 h y

        titleText =
            "(" ++ round3DecPlaces x ++ ", " ++ round3DecPlaces y ++ ")"
    in
    Svg.circle
        [ SA.cx (toStr cx_)
        , SA.cy (toStr cy_)
        , SA.r "5"
        , SA.fill "black"
        , onMouseDownPos (StartDrag i)
        , SE.onMouseOver (Hover i)
        , SE.onMouseOut Unhover
        ]
        [ Svg.title []
            [ Svg.text titleText ]
        ]


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

        totalCov =
            List.foldl
                (\( x, y ) acc -> acc + (x - meanX) * (y - meanY))
                0
                model.points
    in
    div [ HA.style "margin" "1rem" ]
        [ div []
            [ text ("Cov(X,Y) = Σ (xᵢ - x̄)(yᵢ - ȳ) = " ++ round3DecPlaces totalCov) ]
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
                ++ round3DecPlaces dx
                ++ ")·("
                ++ round3DecPlaces dy
                ++ ") = "
                ++ round3DecPlaces product
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
        width =
            800

        height =
            600

        margin =
            40

        ( x, y ) =
            List.head (List.drop i model.points)
                |> Maybe.withDefault ( 0, 0 )
    in
    ( scaleX margin width x, scaleY margin height y )


screenToData : Model -> Float -> Float -> ( Float, Float )
screenToData model screenX screenY =
    let
        width =
            800

        height =
            600

        margin =
            40

        dataX =
            (screenX - toFloat margin) / toFloat (width - 2 * margin)

        dataY =
            (toFloat height - toFloat margin - screenY) / toFloat (height - 2 * margin)
    in
    ( dataX, dataY )


scaleX : Int -> Int -> Float -> Float
scaleX margin width x =
    toFloat margin + x * toFloat (width - 2 * margin)


scaleY : Int -> Int -> Float -> Float
scaleY margin height y =
    (toFloat height - toFloat margin) - y * toFloat (height - 2 * margin)


toStr : Float -> String
toStr =
    String.fromFloat


round3DecPlaces : Float -> String
round3DecPlaces val =
    let
        rounded =
            round (val * 1000)
    in
    String.fromFloat (toFloat rounded / 1000)


clamp : Float -> Float -> Float -> Float
clamp low high x =
    if x < low then
        low

    else if x > high then
        high

    else
        x


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
