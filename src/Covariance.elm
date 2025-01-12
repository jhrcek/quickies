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



-- MODEL


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



-- MESSAGES


type Msg
    = ChangeNumPoints String
    | Generate
    | GotRandomPoints (List ( Float, Float ))
    | Hover Int
    | Unhover
    | StartDrag Int Float Float -- (point index, mouseX, mouseY)
    | Move Float Float -- (mouseX, mouseY)
    | StopDrag



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
                                if 2 <= n && n <= 100 then
                                    Just n

                                else
                                    Nothing
                            )
            in
            pure <|
                case maybeInt of
                    Just validN ->
                        { model | numPoints = validN }

                    Nothing ->
                        model

        Generate ->
            ( model, generatePoints model.numPoints )

        GotRandomPoints newPoints ->
            pure { model | points = newPoints, hoveredIndex = Nothing }

        Hover idx ->
            pure { model | hoveredIndex = Just idx }

        Unhover ->
            pure { model | hoveredIndex = Nothing }

        StartDrag index mouseX mouseY ->
            let
                ( screenX, screenY ) =
                    pointToScreen model index

                offsetX =
                    screenX - mouseX

                offsetY =
                    screenY - mouseY
            in
            pure { model | dragState = Just { index = index, offsetX = offsetX, offsetY = offsetY } }

        Move mouseX mouseY ->
            case model.dragState of
                Just dragState ->
                    let
                        -- new (screen) coordinates for that point
                        newScreenX =
                            mouseX + dragState.offsetX

                        newScreenY =
                            mouseY + dragState.offsetY

                        -- convert screen coords back into data coords
                        ( newDataX, newDataY ) =
                            screenToData model newScreenX newScreenY

                        -- clamp 0..1 if you wish:
                        clampedX =
                            clamp 0 1 newDataX

                        clampedY =
                            clamp 0 1 newDataY

                        updatedPoints =
                            List.indexedMap
                                (\i ( oldX, oldY ) ->
                                    if i == dragState.index then
                                        ( clampedX, clampedY )

                                    else
                                        ( oldX, oldY )
                                )
                                model.points
                    in
                    pure { model | points = updatedPoints }

                Nothing ->
                    -- Not currently dragging, ignore Move
                    ( model, Cmd.none )

        StopDrag ->
            -- Let go of the point
            pure { model | dragState = Nothing }


pure : a -> ( a, Cmd msg )
pure a =
    ( a, Cmd.none )


generatePoints : Int -> Cmd Msg
generatePoints numPoints =
    let
        gen =
            Random.list numPoints (Random.pair (Random.float 0 1) (Random.float 0 1))
    in
    Random.generate GotRandomPoints gen



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onMouseMove (D.map2 Move (D.field "clientX" D.float) (D.field "clientY" D.float))
        , Events.onMouseUp (D.succeed StopDrag)
        ]



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
            scaleX margin width meanX

        meanYSvg =
            scaleY margin height meanY

        hoverRect =
            case model.hoveredIndex of
                Just i ->
                    case List.drop i model.points |> List.head of
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
        ([ -- Axes
           Svg.line
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
         , -- Mean lines
           Svg.line
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
            ++ hoverRect
            ++ List.indexedMap (viewPoint model margin width margin height) model.points
        )


{-| Each point as an SVG circle. We add a custom onMouseDown that gives us the (mouseX, mouseY).
-}
viewPoint :
    Model
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> ( Float, Float )
    -> Svg.Svg Msg
viewPoint model margin w margin2 h i ( x, y ) =
    let
        cx_ =
            scaleX margin w x

        cy_ =
            scaleY margin2 h y

        titleText =
            "(" ++ truncateFloat x ++ ", " ++ truncateFloat y ++ ")"
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
        [ div []
            [ text "Cov(X,Y) = Σ (xᵢ -  x̄) (yᵢ -  ȳ), for i = 1..n" ]
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



-- HELPER FUNCTIONS


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
            case List.drop i model.points |> List.head of
                Just ( px, py ) ->
                    ( px, py )

                Nothing ->
                    ( 0, 0 )
    in
    ( scaleX margin width x
    , scaleY margin height y
    )


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
            (toFloat height - margin - screenY) / toFloat (height - 2 * margin)
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


truncateFloat : Float -> String
truncateFloat val =
    let
        rounded =
            round (val * 100)
    in
    String.fromFloat (toFloat rounded / 100)


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
